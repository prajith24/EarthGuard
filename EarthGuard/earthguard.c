/*
 * EarthGuard - Pollution Pattern Detector
 * Version 1.0
 *
 * A menu-driven console application written in standard C (C11).
 * It does NOT measure pollution. It stores, validates, classifies and
 * compares environmental readings that the user types in manually.
 *
 * Compile (Windows, GCC):
 *   gcc -std=c11 -Wall -Wextra -pedantic -o earthguard.exe earthguard.c -lm
 *
 * Record format in pollution_data.txt (one record per line, '|' separated):
 *   ID|Location|YYYY-MM-DD|AQI|pH|NoiseDB
 *   Example: 1|Model Town Ludhiana|2026-01-15|182|7.40|68.5
 * The '|' character is not allowed inside a location name, so names with
 * spaces are stored and read back safely.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>

/* ------------------------------------------------------------------ */
/* Configuration constants                                             */
/* ------------------------------------------------------------------ */
#define APP_NAME      "EarthGuard - Pollution Pattern Detector"
#define APP_VERSION   "1.0"

#define DATA_FILE     "pollution_data.txt"
#define TEMP_FILE     "pollution_data.tmp"
#define BACKUP_FILE   "pollution_data.bak"
#define REPORT_FILE   "pollution_report.txt"

#define MAX_RECORDS    5000   /* documented capacity limit              */
#define LOCATION_MAX   50     /* includes terminating '\0' (49 chars)   */
#define DATE_LEN       11     /* "YYYY-MM-DD" + '\0'                    */
#define INPUT_BUF      128    /* buffer for one line typed by the user  */
#define LINE_BUF       512    /* buffer for one line of the data file   */
#define MAX_BAD_SHOWN  10     /* malformed line numbers we list         */

/* Input acceptance ranges (basic validation only, NOT safety limits). */
#define AQI_MIN        0
#define AQI_MAX        500    /* illustrative AQI scale tops out at 500 */
#define PH_MIN         0.0    /* mathematical pH range                  */
#define PH_MAX         14.0
#define NOISE_MIN      0.0
#define NOISE_MAX      200.0  /* generous upper bound for typing errors */

/* Illustrative AQI categories (project assumption, NOT official AQI). */
#define AQI_GOOD_MAX       50
#define AQI_SATISFACTORY_MAX 100
#define AQI_MODERATE_MAX   200
#define AQI_POOR_MAX       300
#define AQI_VERY_POOR_MAX  400

/* Project screening rules (configurable, NOT official standards). */
#define AQI_FLAG_ABOVE     200     /* flag AQI above this value          */
#define PH_SCREEN_MIN      6.5     /* pH screening range lower bound     */
#define PH_SCREEN_MAX      8.5     /* pH screening range upper bound     */
#define NOISE_LOW_MAX      55.0
#define NOISE_MODERATE_MAX 70.0
#define NOISE_HIGH_MAX     85.0
#define NOISE_FLAG_ABOVE   70.0    /* flag noise above this value        */

/* Change-detection rules (flags for review, NOT proof of an incident). */
#define AQI_CHANGE_THRESHOLD    50   /* points                           */
#define NOISE_CHANGE_THRESHOLD  10.0 /* dB                               */
#define PH_CHANGE_THRESHOLD     1.0  /* pH units                         */

/* Bit flags for the rules a record can cross. */
#define FLAG_AQI    1
#define FLAG_PH     2
#define FLAG_NOISE  4

/* ------------------------------------------------------------------ */
/* Data structures                                                     */
/* ------------------------------------------------------------------ */
typedef struct {
    int    id;
    char   location[LOCATION_MAX];
    char   date[DATE_LEN];
    int    aqi;
    double ph;
    double noise;
} Record;

typedef struct {
    Record *items;
    size_t  count;
    size_t  capacity;
} Database;

typedef struct {
    int    fileMissing;
    size_t malformed;
    long   badLines[MAX_BAD_SHOWN];
    int    truncated;
} LoadInfo;

typedef struct {
    size_t total;
    size_t locations;
    size_t flagged;
    size_t flagAqi, flagPh, flagNoise;
    double avgAqi, avgPh, avgNoise;
    int    maxAqi;
    double maxNoise;
    double minPh, maxPh;
} Stats;

enum { IN_OK = 0, IN_TOO_LONG = 1, IN_EOF = 2 };
enum { NUM_OK = 0, NUM_NOT_NUMBER = 1, NUM_OUT_OF_RANGE = 2 };

static Database g_db = { NULL, 0, 0 };
static int g_sortField = 1;
static int g_sortAscending = 1;

/* ------------------------------------------------------------------ */
/* Function prototypes                                                 */
/* ------------------------------------------------------------------ */
static void displayHeader(void);
static void displayMenu(void);
static void freeResources(Database *db);
static void handleEof(void);

static int  readLine(char *buf, size_t size);
static void promptLine(const char *prompt, char *buf, size_t size, int allowEmpty);
static int  promptInt(const char *prompt, int min, int max);
static double promptDouble(const char *prompt, double min, double max);
static void promptLocation(char *out);
static void promptDate(const char *prompt, char *out);
static void pressEnter(void);
static int  validateNumericInput(const char *text, double min, double max, double *out);
static int  parseLongStrict(const char *text, long *out);
static int  validateLocation(const char *name);
static int  isLeapYear(int year);
static int  daysInMonth(int year, int month);
static int  validateDate(const char *date);

static int  equalsIgnoreCase(const char *a, const char *b);
static int  compareIgnoreCase(const char *a, const char *b);
static int  containsIgnoreCase(const char *hay, const char *needle);

static const char *classifyAQI(int aqi);
static const char *explainAQI(int aqi);
static const char *classifyPH(double ph);
static const char *explainPH(double ph);
static const char *classifyNoise(double db);
static const char *explainNoise(double db);
static int  flagMask(const Record *r);

static int  dbAppend(Database *db, const Record *r);
static int  findIndexById(const Database *db, int id);
static int  nextRecordId(const Database *db);
static int  parseRecordLine(char *line, Record *r);
static int  loadRecords(const char *path, Database *db, LoadInfo *info);
static int  writeRecordLine(FILE *fp, const Record *r);
static int  appendRecordToFile(const Record *r);
static int  saveRecords(const Database *db);

static void printTableHeader(void);
static void printRecordRow(const Record *r);
static void printFlagText(FILE *out, int mask);
static void collectValues(Record *r);
static void addReading(void);
static void viewRecords(void);
static void analyzeReading(const Record *r);
static void analyzeMenu(void);
static int  isFirstOccurrence(const Database *db, size_t index);
static size_t countUniqueLocations(const Database *db);
static void listLocations(void);
static int  findLatestForLocation(const char *loc);
static int  findByLocationDate(const char *loc, const char *date);
static int  pickLocationReading(const char *label, int *outIndex);
static void compareLocations(void);
static int  analyzeChanges(FILE *out);
static void detectChanges(void);
static void searchRecords(void);
static int  compareRecords(const void *a, const void *b);
static void sortRecords(void);
static void computeStats(const Database *db, Stats *s);
static void writeLocationSummary(FILE *out);
static int  generateReport(void);
static void displaySummary(void);
static void displayProjectInfo(void);

/* ------------------------------------------------------------------ */
/* Basic helpers                                                       */
/* ------------------------------------------------------------------ */
static void freeResources(Database *db)
{
    free(db->items);
    db->items = NULL;
    db->count = 0;
    db->capacity = 0;
}

static void handleEof(void)
{
    printf("\nInput ended unexpectedly. Exiting safely.\n");
    freeResources(&g_db);
    exit(EXIT_FAILURE);
}

static int equalsIgnoreCase(const char *a, const char *b)
{
    return compareIgnoreCase(a, b) == 0;
}

static int compareIgnoreCase(const char *a, const char *b)
{
    while (*a && *b) {
        int ca = tolower((unsigned char)*a);
        int cb = tolower((unsigned char)*b);
        if (ca != cb) {
            return ca - cb;
        }
        a++;
        b++;
    }
    return tolower((unsigned char)*a) - tolower((unsigned char)*b);
}

static int containsIgnoreCase(const char *hay, const char *needle)
{
    size_t nlen = strlen(needle);
    size_t hlen = strlen(hay);
    size_t i, j;
    if (nlen == 0) {
        return 1;
    }
    if (nlen > hlen) {
        return 0;
    }
    for (i = 0; i + nlen <= hlen; i++) {
        for (j = 0; j < nlen; j++) {
            if (tolower((unsigned char)hay[i + j]) !=
                tolower((unsigned char)needle[j])) {
                break;
            }
        }
        if (j == nlen) {
            return 1;
        }
    }
    return 0;
}

/* ------------------------------------------------------------------ */
/* Safe keyboard input                                                 */
/* ------------------------------------------------------------------ */

/* Reads one line, removes the newline and surrounding spaces. If the line
 * is longer than the buffer, the rest is discarded and IN_TOO_LONG is returned. */
static int readLine(char *buf, size_t size)
{
    size_t len, start;
    if (fgets(buf, (int)size, stdin) == NULL) {
        return IN_EOF;
    }
    len = strlen(buf);
    if (len > 0 && buf[len - 1] == '\n') {
        buf[len - 1] = '\0';
    } else {
        int c, extra = 0;
        while ((c = getchar()) != '\n' && c != EOF) {
            extra = 1;
        }
        if (extra) {
            buf[0] = '\0';
            return IN_TOO_LONG;
        }
    }
    len = strlen(buf);
    while (len > 0 && isspace((unsigned char)buf[len - 1])) {
        buf[--len] = '\0';
    }
    start = 0;
    while (buf[start] != '\0' && isspace((unsigned char)buf[start])) {
        start++;
    }
    if (start > 0) {
        memmove(buf, buf + start, strlen(buf + start) + 1);
    }
    return IN_OK;
}

static void promptLine(const char *prompt, char *buf, size_t size, int allowEmpty)
{
    for (;;) {
        int status;
        printf("%s", prompt);
        fflush(stdout);
        status = readLine(buf, size);
        if (status == IN_EOF) {
            handleEof();
        }
        if (status == IN_TOO_LONG) {
            printf("  Error: input is too long (maximum %d characters). Try again.\n",
                   (int)size - 1);
            continue;
        }
        if (!allowEmpty && buf[0] == '\0') {
            printf("  Error: input cannot be empty. Try again.\n");
            continue;
        }
        return;
    }
}

static void pressEnter(void)
{
    char buf[INPUT_BUF];
    printf("\nPress Enter to continue...");
    fflush(stdout);
    if (readLine(buf, sizeof buf) == IN_EOF) {
        handleEof();
    }
}

static int parseLongStrict(const char *text, long *out)
{
    char *end;
    long value;
    errno = 0;
    value = strtol(text, &end, 10);
    if (end == text || *end != '\0' || errno == ERANGE) {
        return 0;
    }
    *out = value;
    return 1;
}

/* Checks that text is a finite number inside [min, max]. */
static int validateNumericInput(const char *text, double min, double max, double *out)
{
    char *end;
    double value;
    errno = 0;
    value = strtod(text, &end);
    if (end == text || *end != '\0' || errno == ERANGE || !isfinite(value)) {
        return NUM_NOT_NUMBER;
    }
    if (value < min || value > max) {
        return NUM_OUT_OF_RANGE;
    }
    *out = value;
    return NUM_OK;
}

static int promptInt(const char *prompt, int min, int max)
{
    char buf[INPUT_BUF];
    for (;;) {
        long value;
        promptLine(prompt, buf, sizeof buf, 0);
        if (!parseLongStrict(buf, &value)) {
            printf("  Error: please type a whole number (no letters or decimals).\n");
            continue;
        }
        if (value < min || value > max) {
            printf("  Error: value must be between %d and %d.\n", min, max);
            continue;
        }
        return (int)value;
    }
}

static double promptDouble(const char *prompt, double min, double max)
{
    char buf[INPUT_BUF];
    for (;;) {
        double value = 0.0;
        int status;
        promptLine(prompt, buf, sizeof buf, 0);
        status = validateNumericInput(buf, min, max, &value);
        if (status == NUM_NOT_NUMBER) {
            printf("  Error: please type a number (for example 7.2).\n");
            continue;
        }
        if (status == NUM_OUT_OF_RANGE) {
            printf("  Error: value must be between %.1f and %.1f.\n", min, max);
            continue;
        }
        return value;
    }
}

/* ------------------------------------------------------------------ */
/* Date and location validation                                        */
/* ------------------------------------------------------------------ */
static int isLeapYear(int year)
{
    return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
}

static int daysInMonth(int year, int month)
{
    static const int days[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
    if (month == 2 && isLeapYear(year)) {
        return 29;
    }
    return days[month - 1];
}

/* Returns 1 if date is a real calendar date in YYYY-MM-DD form. */
static int validateDate(const char *date)
{
    int i, year, month, day;
    if (strlen(date) != 10) {
        return 0;
    }
    for (i = 0; i < 10; i++) {
        if (i == 4 || i == 7) {
            if (date[i] != '-') {
                return 0;
            }
        } else if (!isdigit((unsigned char)date[i])) {
            return 0;
        }
    }
    year  = (date[0] - '0') * 1000 + (date[1] - '0') * 100 +
            (date[2] - '0') * 10 + (date[3] - '0');
    month = (date[5] - '0') * 10 + (date[6] - '0');
    day   = (date[8] - '0') * 10 + (date[9] - '0');
    if (year < 1900 || year > 2100 || month < 1 || month > 12 || day < 1) {
        return 0;
    }
    return day <= daysInMonth(year, month);
}

/* Returns 1 if the location name is acceptable. */
static int validateLocation(const char *name)
{
    size_t i, len = strlen(name);
    if (len == 0 || len >= LOCATION_MAX) {
        return 0;
    }
    for (i = 0; i < len; i++) {
        if (!isprint((unsigned char)name[i]) || name[i] == '|') {
            return 0;
        }
    }
    return 1;
}

static void promptLocation(char *out)
{
    char buf[INPUT_BUF];
    for (;;) {
        promptLine("  Location name (spaces allowed): ", buf, sizeof buf, 0);
        if (strlen(buf) >= LOCATION_MAX) {
            printf("  Error: location name is too long (maximum %d characters).\n",
                   LOCATION_MAX - 1);
            continue;
        }
        if (!validateLocation(buf)) {
            printf("  Error: location may not contain the '|' character or control characters.\n");
            continue;
        }
        memcpy(out, buf, strlen(buf) + 1); /* length already checked above */
        return;
    }
}

static void promptDate(const char *prompt, char *out)
{
    char buf[INPUT_BUF];
    for (;;) {
        promptLine(prompt, buf, sizeof buf, 0);
        if (!validateDate(buf)) {
            printf("  Error: invalid date. Use YYYY-MM-DD with a real date (year 1900-2100).\n");
            continue;
        }
        memcpy(out, buf, DATE_LEN); /* validateDate guarantees exactly 10 chars + NUL */
        return;
    }
}

/* ------------------------------------------------------------------ */
/* Classification (documented project assumptions)                     */
/* ------------------------------------------------------------------ */
static const char *classifyAQI(int aqi)
{
    if (aqi <= AQI_GOOD_MAX)         return "Good";
    if (aqi <= AQI_SATISFACTORY_MAX) return "Satisfactory";
    if (aqi <= AQI_MODERATE_MAX)     return "Moderate";
    if (aqi <= AQI_POOR_MAX)         return "Poor";
    if (aqi <= AQI_VERY_POOR_MAX)    return "Very Poor";
    return "Severe";
}

static const char *explainAQI(int aqi)
{
    if (aqi <= AQI_GOOD_MAX)
        return "Air quality is in the lowest band of this illustrative scale.";
    if (aqi <= AQI_SATISFACTORY_MAX)
        return "Slightly elevated; sensitive people may notice effects.";
    if (aqi <= AQI_MODERATE_MAX)
        return "Moderately elevated; prolonged exposure may affect sensitive groups.";
    if (aqi <= AQI_POOR_MAX)
        return "Elevated level; this project flags it for verification.";
    if (aqi <= AQI_VERY_POOR_MAX)
        return "High level on this scale; flagged for verification.";
    return "Highest band of this scale; flagged for verification.";
}

static const char *classifyPH(double ph)
{
    if (ph < PH_SCREEN_MIN) return "Below range";
    if (ph > PH_SCREEN_MAX) return "Above range";
    return "In range";
}

static const char *explainPH(double ph)
{
    if (ph < PH_SCREEN_MIN)
        return "Below the project screening range (more acidic). Verify with a calibrated test.";
    if (ph > PH_SCREEN_MAX)
        return "Above the project screening range (more alkaline). Verify with a calibrated test.";
    return "Inside the project screening range. This alone does not prove water is safe.";
}

static const char *classifyNoise(double db)
{
    if (db <= NOISE_LOW_MAX)      return "Low";
    if (db <= NOISE_MODERATE_MAX) return "Moderate";
    if (db <= NOISE_HIGH_MAX)     return "High";
    return "Very High";
}

static const char *explainNoise(double db)
{
    if (db <= NOISE_LOW_MAX)
        return "At or below the project's low-noise threshold.";
    if (db <= NOISE_MODERATE_MAX)
        return "Moderate level; impact depends on duration and surroundings.";
    if (db <= NOISE_HIGH_MAX)
        return "Above the project flag threshold; impact depends on duration and setting.";
    return "Well above the project flag threshold; verify the measurement and setting.";
}

/* Returns a bit mask of the project rules this record crosses. */
static int flagMask(const Record *r)
{
    int mask = 0;
    if (r->aqi > AQI_FLAG_ABOVE) {
        mask |= FLAG_AQI;
    }
    if (r->ph < PH_SCREEN_MIN || r->ph > PH_SCREEN_MAX) {
        mask |= FLAG_PH;
    }
    if (r->noise > NOISE_FLAG_ABOVE) {
        mask |= FLAG_NOISE;
    }
    return mask;
}

/* ------------------------------------------------------------------ */
/* Database and file handling                                          */
/* ------------------------------------------------------------------ */
static int dbAppend(Database *db, const Record *r)
{
    if (db->count >= MAX_RECORDS) {
        return 0;
    }
    if (db->count == db->capacity) {
        size_t newCap = db->capacity ? db->capacity * 2 : 64;
        Record *p;
        if (newCap > MAX_RECORDS) {
            newCap = MAX_RECORDS;
        }
        p = realloc(db->items, newCap * sizeof(Record));
        if (p == NULL) {
            return 0;
        }
        db->items = p;
        db->capacity = newCap;
    }
    db->items[db->count++] = *r;
    return 1;
}

static int findIndexById(const Database *db, int id)
{
    size_t i;
    for (i = 0; i < db->count; i++) {
        if (db->items[i].id == id) {
            return (int)i;
        }
    }
    return -1;
}

static int nextRecordId(const Database *db)
{
    size_t i;
    int maxId = 0;
    for (i = 0; i < db->count; i++) {
        if (db->items[i].id > maxId) {
            maxId = db->items[i].id;
        }
    }
    return maxId + 1;
}

/* Splits one stored line (modified in place) and validates every field. */
static int parseRecordLine(char *line, Record *r)
{
    char *fields[6];
    int n = 0;
    char *p = line;
    long id, aqi;
    double ph, noise;

    fields[n++] = p;
    while (*p) {
        if (*p == '|') {
            *p = '\0';
            if (n >= 6) {
                return 0;
            }
            fields[n++] = p + 1;
        }
        p++;
    }
    if (n != 6) {
        return 0;
    }
    if (!parseLongStrict(fields[0], &id) || id < 1 || id > 2000000000L) {
        return 0;
    }
    if (!validateLocation(fields[1]) || !validateDate(fields[2])) {
        return 0;
    }
    if (!parseLongStrict(fields[3], &aqi) || aqi < AQI_MIN || aqi > AQI_MAX) {
        return 0;
    }
    if (validateNumericInput(fields[4], PH_MIN, PH_MAX, &ph) != NUM_OK) {
        return 0;
    }
    if (validateNumericInput(fields[5], NOISE_MIN, NOISE_MAX, &noise) != NUM_OK) {
        return 0;
    }
    r->id = (int)id;
    snprintf(r->location, sizeof r->location, "%s", fields[1]);
    snprintf(r->date, sizeof r->date, "%s", fields[2]);
    r->aqi = (int)aqi;
    r->ph = ph;
    r->noise = noise;
    return 1;
}

/* Loads the data file into db. Returns 1 on success (a missing file counts as
 * an empty database and is reported in info), 0 on a real read/open error. */
static int loadRecords(const char *path, Database *db, LoadInfo *info)
{
    FILE *fp;
    char line[LINE_BUF];
    long lineNo = 0;

    memset(info, 0, sizeof *info);
    fp = fopen(path, "r");
    if (fp == NULL) {
        if (errno == ENOENT) {
            info->fileMissing = 1;
            return 1;
        }
        printf("  Error: cannot open %s (%s).\n", path, strerror(errno));
        return 0;
    }
    while (fgets(line, sizeof line, fp) != NULL) {
        size_t len = strlen(line);
        int complete = (len > 0 && line[len - 1] == '\n');
        Record rec;
        int bad = 0;

        lineNo++;
        if (!complete && !feof(fp)) {
            int c;
            while ((c = fgetc(fp)) != '\n' && c != EOF) {
                /* discard the rest of an over-long line */
            }
            bad = 1;
        } else {
            while (len > 0 && (line[len - 1] == '\n' || line[len - 1] == '\r')) {
                line[--len] = '\0';
            }
            if (len == 0) {
                continue; /* ignore blank lines */
            }
            if (!parseRecordLine(line, &rec) || findIndexById(db, rec.id) >= 0) {
                bad = 1;
            } else if (db->count >= MAX_RECORDS) {
                info->truncated = 1;
                break;
            } else if (!dbAppend(db, &rec)) {
                printf("  Error: out of memory while loading records.\n");
                fclose(fp);
                return 0;
            }
        }
        if (bad) {
            if (info->malformed < MAX_BAD_SHOWN) {
                info->badLines[info->malformed] = lineNo;
            }
            info->malformed++;
        }
    }
    if (ferror(fp)) {
        printf("  Error: a read error occurred while reading %s.\n", path);
        fclose(fp);
        return 0;
    }
    fclose(fp);
    return 1;
}

static void reportLoadInfo(const LoadInfo *info)
{
    size_t i;
    if (info->malformed > 0) {
        printf("  Warning: %lu malformed line(s) were skipped (line numbers:",
               (unsigned long)info->malformed);
        for (i = 0; i < info->malformed && i < MAX_BAD_SHOWN; i++) {
            printf(" %ld", info->badLines[i]);
        }
        if (info->malformed > MAX_BAD_SHOWN) {
            printf(" ...");
        }
        printf(").\n  The file itself was not changed.\n");
    }
    if (info->truncated) {
        printf("  Warning: file has more than %d records; extra records were not loaded.\n",
               MAX_RECORDS);
    }
}

static int writeRecordLine(FILE *fp, const Record *r)
{
    return fprintf(fp, "%d|%s|%s|%d|%.2f|%.1f\n",
                   r->id, r->location, r->date, r->aqi, r->ph, r->noise) > 0;
}

/* Adds one record at the end of the data file; existing lines are untouched. */
static int appendRecordToFile(const Record *r)
{
    FILE *fp = fopen(DATA_FILE, "a");
    if (fp == NULL) {
        printf("  Error: cannot open %s for writing (%s).\n", DATA_FILE, strerror(errno));
        return 0;
    }
    if (!writeRecordLine(fp, r)) {
        printf("  Error: failed while writing to %s.\n", DATA_FILE);
        fclose(fp);
        return 0;
    }
    if (fclose(fp) != 0) {
        printf("  Error: failed to finish writing %s.\n", DATA_FILE);
        return 0;
    }
    return 1;
}

/* Rewrites the whole data file safely: temp file first, then replace.
 * On Windows rename() fails if the target exists, so the old file is first
 * renamed to a backup and restored if the replacement fails. */
static int saveRecords(const Database *db)
{
    FILE *fp;
    size_t i;
    int hadOriginal;
    FILE *probe;

    fp = fopen(TEMP_FILE, "w");
    if (fp == NULL) {
        printf("  Error: cannot create %s (%s). Original data not changed.\n",
               TEMP_FILE, strerror(errno));
        return 0;
    }
    for (i = 0; i < db->count; i++) {
        if (!writeRecordLine(fp, &db->items[i])) {
            printf("  Error: write failed. Original data not changed.\n");
            fclose(fp);
            remove(TEMP_FILE);
            return 0;
        }
    }
    if (fflush(fp) != 0 || fclose(fp) != 0) {
        printf("  Error: could not finish writing temp file. Original data not changed.\n");
        remove(TEMP_FILE);
        return 0;
    }

    probe = fopen(DATA_FILE, "r");
    hadOriginal = (probe != NULL);
    if (probe != NULL) {
        fclose(probe);
    }
    if (hadOriginal) {
        remove(BACKUP_FILE);
        if (rename(DATA_FILE, BACKUP_FILE) != 0) {
            printf("  Error: cannot create backup (%s). Original data not changed.\n",
                   strerror(errno));
            remove(TEMP_FILE);
            return 0;
        }
    }
    if (rename(TEMP_FILE, DATA_FILE) != 0) {
        printf("  Error: cannot replace data file (%s).\n", strerror(errno));
        if (hadOriginal && rename(BACKUP_FILE, DATA_FILE) != 0) {
            printf("  Your original data is safe in %s.\n", BACKUP_FILE);
        }
        remove(TEMP_FILE);
        return 0;
    }
    if (hadOriginal) {
        remove(BACKUP_FILE);
    }
    return 1;
}

/* ------------------------------------------------------------------ */
/* Display helpers                                                     */
/* ------------------------------------------------------------------ */
static void displayHeader(void)
{
    printf("\n==============================================================\n");
    printf("   EARTHGUARD - POLLUTION PATTERN DETECTOR      Version %s\n", APP_VERSION);
    printf("   Analyses manually entered data. Not a certified instrument.\n");
    printf("==============================================================\n");
}

static void displayMenu(void)
{
    printf("\n------------------------ MAIN MENU ---------------------------\n");
    printf("  1. Add New Pollution Reading\n");
    printf("  2. View All Saved Readings\n");
    printf("  3. Analyze a Pollution Reading\n");
    printf("  4. Compare Pollution Between Locations\n");
    printf("  5. Detect Unusual Pollution Changes\n");
    printf("  6. Search Pollution Records\n");
    printf("  7. Sort Pollution Records\n");
    printf("  8. Generate Pollution Report\n");
    printf("  9. View Pollution Summary\n");
    printf(" 10. View Project Information\n");
    printf(" 11. Exit Application\n");
    printf("--------------------------------------------------------------\n");
}

static void printTableHeader(void)
{
    printf("%-5s %-24s %-10s %5s %6s %7s  %-12s %-11s %-10s %s\n",
           "ID", "Location", "Date", "AQI", "pH", "dB",
           "AQI Category", "pH Screen", "Noise", "Flag");
    printf("------------------------------------------------------------"
           "------------------------------------------\n");
}

static void printRecordRow(const Record *r)
{
    printf("%-5d %-24.24s %-10s %5d %6.2f %7.1f  %-12s %-11s %-10s %s\n",
           r->id, r->location, r->date, r->aqi, r->ph, r->noise,
           classifyAQI(r->aqi), classifyPH(r->ph), classifyNoise(r->noise),
           flagMask(r) ? "YES" : "-");
}

static void printFlagText(FILE *out, int mask)
{
    int first = 1;
    if (mask == 0) {
        fprintf(out, "none");
        return;
    }
    if (mask & FLAG_AQI) {
        fprintf(out, "%sAQI above %d", first ? "" : ", ", AQI_FLAG_ABOVE);
        first = 0;
    }
    if (mask & FLAG_PH) {
        fprintf(out, "%spH outside %.1f-%.1f", first ? "" : ", ", PH_SCREEN_MIN, PH_SCREEN_MAX);
        first = 0;
    }
    if (mask & FLAG_NOISE) {
        fprintf(out, "%snoise above %.0f dB", first ? "" : ", ", NOISE_FLAG_ABOVE);
    }
}

/* ------------------------------------------------------------------ */
/* Menu 1: Add reading                                                 */
/* ------------------------------------------------------------------ */
static void collectValues(Record *r)
{
    char buf[INPUT_BUF];
    r->aqi = promptInt("  AQI value (0-500, whole number): ", AQI_MIN, AQI_MAX);
    r->ph = promptDouble("  Water pH (0-14): ", PH_MIN, PH_MAX);
    for (;;) {
        double v;
        promptLine("  Noise level in dB (0-200): ", buf, sizeof buf, 0);
        if (validateNumericInput(buf, NOISE_MIN, NOISE_MAX, &v) == NUM_OK) {
            r->noise = v;
            break;
        }
        if (buf[0] == '-') {
            printf("  Error: noise cannot be negative.\n");
        } else {
            printf("  Error: type a number between 0 and 200.\n");
        }
    }
}

static void addReading(void)
{
    Record rec;
    printf("\n--- Add New Pollution Reading ---\n");
    if (g_db.count >= MAX_RECORDS) {
        printf("  Error: capacity reached (%d records). No more records can be added.\n",
               MAX_RECORDS);
        return;
    }
    memset(&rec, 0, sizeof rec);
    promptLocation(rec.location);
    promptDate("  Date (YYYY-MM-DD): ", rec.date);
    collectValues(&rec);
    rec.id = nextRecordId(&g_db);
    if (findIndexById(&g_db, rec.id) >= 0) {
        printf("  Error: duplicate record ID. Record not saved.\n");
        return;
    }
    if (!appendRecordToFile(&rec)) {
        printf("  Record NOT saved.\n");
        return;
    }
    if (!dbAppend(&g_db, &rec)) {
        printf("  Warning: saved to file but memory is full; restart the program to reload.\n");
        return;
    }
    printf("  Success: record %d saved to %s.\n", rec.id, DATA_FILE);
    if (flagMask(&rec)) {
        printf("  Note: this reading crosses a project rule (");
        printFlagText(stdout, flagMask(&rec));
        printf("). Use option 3 for details.\n");
    }
}

/* ------------------------------------------------------------------ */
/* Menu 2: View records                                                */
/* ------------------------------------------------------------------ */
static void viewRecords(void)
{
    Database fresh = { NULL, 0, 0 };
    LoadInfo info;
    size_t i;

    printf("\n--- All Saved Readings ---\n");
    if (!loadRecords(DATA_FILE, &fresh, &info)) {
        printf("  The data file could not be read, so records cannot be shown.\n");
        printf("  (Your data was not changed and is NOT treated as empty.)\n");
        freeResources(&fresh);
        return;
    }
    reportLoadInfo(&info);
    if (info.fileMissing || fresh.count == 0) {
        printf("  No records found. Use option 1 to add a reading.\n");
        freeResources(&fresh);
        return;
    }
    freeResources(&g_db);
    g_db = fresh;
    printf("  Records in file order. Flag = crosses a project screening rule.\n\n");
    printTableHeader();
    for (i = 0; i < g_db.count; i++) {
        printRecordRow(&g_db.items[i]);
    }
    printf("\n  Total records: %lu\n", (unsigned long)g_db.count);
}

/* ------------------------------------------------------------------ */
/* Menu 3: Analyze                                                     */
/* ------------------------------------------------------------------ */
static void analyzeReading(const Record *r)
{
    int mask = flagMask(r);
    printf("\n  ---- Analysis ----\n");
    printf("  Location : %s\n", r->location[0] ? r->location : "(not saved)");
    printf("  Date     : %s\n", r->date[0] ? r->date : "(not set)");
    printf("  AQI      : %d  -> %s\n", r->aqi, classifyAQI(r->aqi));
    printf("             %s\n", explainAQI(r->aqi));
    printf("             (Illustrative project scale, not the official Indian AQI.)\n");
    printf("  Water pH : %.2f -> %s (screening range %.1f-%.1f)\n",
           r->ph, classifyPH(r->ph), PH_SCREEN_MIN, PH_SCREEN_MAX);
    printf("             %s\n", explainPH(r->ph));
    printf("             pH is only one water-quality indicator.\n");
    printf("  Noise    : %.1f dB -> %s\n", r->noise, classifyNoise(r->noise));
    printf("             %s\n", explainNoise(r->noise));
    printf("  Flagged  : ");
    printFlagText(stdout, mask);
    printf("\n");
    if (mask) {
        printf("  Recommendation: verify flagged values with calibrated instruments or\n"
               "  the applicable official standard before drawing conclusions.\n");
    } else {
        printf("  No project rule was crossed. This is not a safety guarantee.\n");
    }
    printf("  (No medical, legal or compliance conclusion is made by this program.)\n");
}

static void analyzeMenu(void)
{
    int choice;
    printf("\n--- Analyze a Pollution Reading ---\n");
    printf("  1. Analyze a saved record (by ID)\n");
    printf("  2. Enter values now and analyze\n");
    choice = promptInt("  Choose 1 or 2: ", 1, 2);
    if (choice == 1) {
        int id, idx;
        if (g_db.count == 0) {
            printf("  No saved records. Add a reading first, or choose option 2.\n");
            return;
        }
        id = promptInt("  Record ID: ", 1, 2000000000);
        idx = findIndexById(&g_db, id);
        if (idx < 0) {
            printf("  Error: record ID %d does not exist.\n", id);
            return;
        }
        analyzeReading(&g_db.items[idx]);
    } else {
        Record rec;
        char buf[INPUT_BUF];
        memset(&rec, 0, sizeof rec);
        collectValues(&rec);
        analyzeReading(&rec);
        promptLine("\n  Save this as a new record? (y/n): ", buf, sizeof buf, 0);
        if (buf[0] == 'y' || buf[0] == 'Y') {
            if (g_db.count >= MAX_RECORDS) {
                printf("  Error: capacity reached. Record not saved.\n");
                return;
            }
            promptLocation(rec.location);
            promptDate("  Date (YYYY-MM-DD): ", rec.date);
            rec.id = nextRecordId(&g_db);
            if (appendRecordToFile(&rec) && dbAppend(&g_db, &rec)) {
                printf("  Success: record %d saved.\n", rec.id);
            } else {
                printf("  Record NOT saved.\n");
            }
        }
    }
}

/* ------------------------------------------------------------------ */
/* Location helpers                                                    */
/* ------------------------------------------------------------------ */
static int isFirstOccurrence(const Database *db, size_t index)
{
    size_t i;
    for (i = 0; i < index; i++) {
        if (equalsIgnoreCase(db->items[i].location, db->items[index].location)) {
            return 0;
        }
    }
    return 1;
}

static size_t countUniqueLocations(const Database *db)
{
    size_t i, n = 0;
    for (i = 0; i < db->count; i++) {
        if (isFirstOccurrence(db, i)) {
            n++;
        }
    }
    return n;
}

static void listLocations(void)
{
    size_t i;
    printf("  Known locations:\n");
    for (i = 0; i < g_db.count; i++) {
        if (isFirstOccurrence(&g_db, i)) {
            printf("    - %s\n", g_db.items[i].location);
        }
    }
}

/* Latest reading (by date, then ID) for a location, or -1. */
static int findLatestForLocation(const char *loc)
{
    size_t i;
    int best = -1;
    for (i = 0; i < g_db.count; i++) {
        if (equalsIgnoreCase(g_db.items[i].location, loc)) {
            if (best < 0) {
                best = (int)i;
            } else {
                int c = strcmp(g_db.items[i].date, g_db.items[best].date);
                if (c > 0 || (c == 0 && g_db.items[i].id > g_db.items[best].id)) {
                    best = (int)i;
                }
            }
        }
    }
    return best;
}

static int findByLocationDate(const char *loc, const char *date)
{
    size_t i;
    int best = -1;
    for (i = 0; i < g_db.count; i++) {
        if (equalsIgnoreCase(g_db.items[i].location, loc) &&
            strcmp(g_db.items[i].date, date) == 0) {
            if (best < 0 || g_db.items[i].id > g_db.items[best].id) {
                best = (int)i;
            }
        }
    }
    return best;
}

/* Asks for a location (and optional date). Returns 1 and the record index. */
static int pickLocationReading(const char *label, int *outIndex)
{
    char loc[INPUT_BUF], date[INPUT_BUF];
    int idx;
    printf("\n  %s\n", label);
    promptLine("  Location name: ", loc, sizeof loc, 0);
    if (findLatestForLocation(loc) < 0) {
        printf("  Error: unknown location \"%s\".\n", loc);
        return 0;
    }
    promptLine("  Date YYYY-MM-DD (press Enter for latest reading): ", date, sizeof date, 1);
    if (date[0] == '\0') {
        idx = findLatestForLocation(loc);
    } else {
        if (!validateDate(date)) {
            printf("  Error: invalid date.\n");
            return 0;
        }
        idx = findByLocationDate(loc, date);
        if (idx < 0) {
            printf("  Error: no reading for \"%s\" on %s.\n", loc, date);
            return 0;
        }
    }
    *outIndex = idx;
    return 1;
}

/* ------------------------------------------------------------------ */
/* Menu 4: Compare locations                                           */
/* ------------------------------------------------------------------ */
static void compareLocations(void)
{
    int a, b;
    const Record *ra, *rb;
    printf("\n--- Compare Pollution Between Locations ---\n");
    if (g_db.count == 0) {
        printf("  No records available. Add readings first.\n");
        return;
    }
    listLocations();
    if (!pickLocationReading("First location:", &a)) {
        return;
    }
    if (!pickLocationReading("Second location:", &b)) {
        return;
    }
    ra = &g_db.items[a];
    rb = &g_db.items[b];
    if (equalsIgnoreCase(ra->location, rb->location)) {
        printf("  Error: please choose two different locations.\n");
        return;
    }
    printf("\n  %-14s | %-24.24s | %-24.24s | Difference (B - A)\n", "", ra->location, rb->location);
    printf("  --------------------------------------------------------------------------------\n");
    printf("  %-14s | %-24s | %-24s |\n", "Reading date", ra->date, rb->date);
    printf("  %-14s | %-24d | %-24d | %+d\n", "AQI", ra->aqi, rb->aqi, rb->aqi - ra->aqi);
    printf("  %-14s | %-24s | %-24s |\n", "AQI category", classifyAQI(ra->aqi), classifyAQI(rb->aqi));
    printf("  %-14s | %-24.2f | %-24.2f | %+.2f\n", "Water pH", ra->ph, rb->ph, rb->ph - ra->ph);
    printf("  %-14s | %-24s | %-24s |\n", "pH screening", classifyPH(ra->ph), classifyPH(rb->ph));
    printf("  %-14s | %-24.1f | %-24.1f | %+.1f\n", "Noise (dB)", ra->noise, rb->noise, rb->noise - ra->noise);
    printf("\n  Higher AQI  : %s\n",
           ra->aqi == rb->aqi ? "Equal" : (ra->aqi > rb->aqi ? ra->location : rb->location));
    printf("  Higher noise: %s\n",
           ra->noise == rb->noise ? "Equal" : (ra->noise > rb->noise ? ra->location : rb->location));
    if (flagMask(ra) & FLAG_PH) {
        printf("  pH of %s is outside the screening range %.1f-%.1f.\n", ra->location, PH_SCREEN_MIN, PH_SCREEN_MAX);
    }
    if (flagMask(rb) & FLAG_PH) {
        printf("  pH of %s is outside the screening range %.1f-%.1f.\n", rb->location, PH_SCREEN_MIN, PH_SCREEN_MAX);
    }
    printf("\n  Note: AQI, pH and dB are different quantities and are NOT combined into one\n"
           "  score. Readings from different dates/places may not be directly comparable.\n");
}

/* ------------------------------------------------------------------ */
/* Menu 5: Detect unusual changes                                      */
/* ------------------------------------------------------------------ */
static void changeIndexSort(const size_t *dummy, size_t *idx, size_t n)
{
    size_t i, j;
    (void)dummy;
    for (i = 1; i < n; i++) {
        size_t key = idx[i];
        j = i;
        while (j > 0) {
            const Record *p = &g_db.items[idx[j - 1]];
            const Record *k = &g_db.items[key];
            int c = strcmp(p->date, k->date);
            if (c > 0 || (c == 0 && p->id > k->id)) {
                idx[j] = idx[j - 1];
                j--;
            } else {
                break;
            }
        }
        idx[j] = key;
    }
}

/* Prints (to out) flagged changes between consecutive readings of each
 * location. Returns the number of flagged pairs, or -1 on memory failure. */
static int analyzeChanges(FILE *out)
{
    size_t *idx;
    size_t i, k, n;
    int totalFlagged = 0;

    idx = malloc((g_db.count ? g_db.count : 1) * sizeof *idx);
    if (idx == NULL) {
        fprintf(out, "  Error: out of memory.\n");
        return -1;
    }
    for (i = 0; i < g_db.count; i++) {
        if (!isFirstOccurrence(&g_db, i)) {
            continue;
        }
        n = 0;
        for (k = 0; k < g_db.count; k++) {
            if (equalsIgnoreCase(g_db.items[k].location, g_db.items[i].location)) {
                idx[n++] = k;
            }
        }
        fprintf(out, "\n  Location: %s (%lu reading%s)\n", g_db.items[i].location,
                (unsigned long)n, n == 1 ? "" : "s");
        if (n < 2) {
            fprintf(out, "    Only one reading: not enough history to compare.\n");
            continue;
        }
        changeIndexSort(NULL, idx, n);
        {
            int pairFlagged = 0;
            for (k = 1; k < n; k++) {
                const Record *o = &g_db.items[idx[k - 1]];
                const Record *c = &g_db.items[idx[k]];
                int dAqi = c->aqi - o->aqi;
                double dNoise = c->noise - o->noise;
                double dPh = c->ph - o->ph;
                int fo = flagMask(o), fc = flagMask(c);
                int aqiBig = abs(dAqi) >= AQI_CHANGE_THRESHOLD;
                int noiseBig = fabs(dNoise) >= NOISE_CHANGE_THRESHOLD;
                int phBig = fabs(dPh) >= PH_CHANGE_THRESHOLD;
                int newCross = fc & ~fo;

                if (!(aqiBig || noiseBig || phBig || newCross)) {
                    continue;
                }
                pairFlagged++;
                totalFlagged++;
                fprintf(out, "    %s (record %d) -> %s (record %d)\n",
                        o->date, o->id, c->date, c->id);
                if (aqiBig) {
                    fprintf(out, "      AQI %s: old %d, new %d, difference %+d",
                            dAqi > 0 ? "increase" : "decrease", o->aqi, c->aqi, dAqi);
                    if (o->aqi != 0) {
                        fprintf(out, " (%+.1f%%)", 100.0 * dAqi / o->aqi);
                    } else {
                        fprintf(out, " (percent n/a: old value is 0)");
                    }
                    fprintf(out, "\n");
                }
                if (noiseBig) {
                    fprintf(out, "      Noise change: old %.1f dB, new %.1f dB, difference %+.1f dB\n",
                            o->noise, c->noise, dNoise);
                }
                if (phBig) {
                    fprintf(out, "      pH change: old %.2f, new %.2f, difference %+.2f\n",
                            o->ph, c->ph, dPh);
                }
                if (newCross) {
                    fprintf(out, "      Newly crossed rule(s): ");
                    printFlagText(out, newCross);
                    fprintf(out, "\n");
                }
            }
            if (pairFlagged == 0) {
                fprintf(out, "    No change exceeded the configured thresholds.\n");
            }
        }
    }
    free(idx);
    return totalFlagged;
}

static void detectChanges(void)
{
    int flagged;
    printf("\n--- Detect Unusual Pollution Changes ---\n");
    if (g_db.count == 0) {
        printf("  No records available. Add readings first.\n");
        return;
    }
    printf("  Rules: AQI change >= %d, noise change >= %.0f dB, pH change >= %.1f,\n"
           "  or a reading newly crossing a screening rule.\n", AQI_CHANGE_THRESHOLD,
           NOISE_CHANGE_THRESHOLD, PH_CHANGE_THRESHOLD);
    flagged = analyzeChanges(stdout);
    if (flagged >= 0) {
        printf("\n  Flagged changes: %d\n", flagged);
        printf("  These are review flags from project rules, NOT confirmed pollution events.\n");
    }
}

/* ------------------------------------------------------------------ */
/* Menu 6: Search                                                      */
/* ------------------------------------------------------------------ */
static void searchRecords(void)
{
    int choice;
    size_t i, found = 0;
    char buf[INPUT_BUF];

    printf("\n--- Search Pollution Records ---\n");
    if (g_db.count == 0) {
        printf("  No records available.\n");
        return;
    }
    printf("  1. By record ID\n  2. By location name (partial match allowed)\n  3. By date\n");
    choice = promptInt("  Choose 1-3: ", 1, 3);
    if (choice == 1) {
        int id = promptInt("  Record ID: ", 1, 2000000000);
        int idx = findIndexById(&g_db, id);
        if (idx < 0) {
            printf("  No record found with ID %d.\n", id);
            return;
        }
        printf("\n");
        printTableHeader();
        printRecordRow(&g_db.items[idx]);
        return;
    }
    if (choice == 2) {
        promptLine("  Location text: ", buf, sizeof buf, 0);
    } else {
        promptDate("  Date (YYYY-MM-DD): ", buf);
    }
    for (i = 0; i < g_db.count; i++) {
        const Record *r = &g_db.items[i];
        int match = (choice == 2) ? containsIgnoreCase(r->location, buf)
                                  : strcmp(r->date, buf) == 0;
        if (match) {
            if (found == 0) {
                printf("\n");
                printTableHeader();
            }
            printRecordRow(r);
            found++;
        }
    }
    if (found == 0) {
        printf("  No matching records found for \"%s\".\n", buf);
    } else {
        printf("\n  %lu record(s) found.\n", (unsigned long)found);
    }
}

/* ------------------------------------------------------------------ */
/* Menu 7: Sort                                                        */
/* ------------------------------------------------------------------ */
static int compareRecords(const void *a, const void *b)
{
    const Record *x = (const Record *)a;
    const Record *y = (const Record *)b;
    int c = 0;
    switch (g_sortField) {
    case 1: c = compareIgnoreCase(x->location, y->location); break;
    case 2: c = strcmp(x->date, y->date); break;
    case 3: c = (x->aqi > y->aqi) - (x->aqi < y->aqi); break;
    case 4: c = (x->ph > y->ph) - (x->ph < y->ph); break;
    case 5: c = (x->noise > y->noise) - (x->noise < y->noise); break;
    default: break;
    }
    if (c == 0) {
        return (x->id > y->id) - (x->id < y->id);
    }
    return g_sortAscending ? c : -c;
}

static void sortRecords(void)
{
    size_t i;
    char buf[INPUT_BUF];
    printf("\n--- Sort Pollution Records ---\n");
    if (g_db.count == 0) {
        printf("  No records available.\n");
        return;
    }
    printf("  Sort by: 1. Location  2. Date  3. AQI  4. Water pH  5. Noise\n");
    g_sortField = promptInt("  Choose 1-5: ", 1, 5);
    printf("  Order:   1. Ascending  2. Descending\n");
    g_sortAscending = (promptInt("  Choose 1-2: ", 1, 2) == 1);
    qsort(g_db.items, g_db.count, sizeof(Record), compareRecords);
    printf("\n");
    printTableHeader();
    for (i = 0; i < g_db.count; i++) {
        printRecordRow(&g_db.items[i]);
    }
    printf("\n  Sorted IN MEMORY only. The data file is unchanged unless you choose to save.\n");
    promptLine("  Save this order permanently to the data file? (y/n): ", buf, sizeof buf, 0);
    if (buf[0] == 'y' || buf[0] == 'Y') {
        if (saveRecords(&g_db)) {
            printf("  Success: data file rewritten in the new order.\n");
        } else {
            printf("  Data file was not changed.\n");
        }
    } else {
        printf("  Data file left unchanged.\n");
    }
}

/* ------------------------------------------------------------------ */
/* Statistics, summary and report                                      */
/* ------------------------------------------------------------------ */
static void computeStats(const Database *db, Stats *s)
{
    size_t i;
    double sumAqi = 0.0, sumPh = 0.0, sumNoise = 0.0;
    memset(s, 0, sizeof *s);
    s->total = db->count;
    if (db->count == 0) {
        return;
    }
    s->maxAqi = db->items[0].aqi;
    s->maxNoise = db->items[0].noise;
    s->minPh = s->maxPh = db->items[0].ph;
    for (i = 0; i < db->count; i++) {
        const Record *r = &db->items[i];
        int mask = flagMask(r);
        sumAqi += r->aqi;
        sumPh += r->ph;
        sumNoise += r->noise;
        if (r->aqi > s->maxAqi) s->maxAqi = r->aqi;
        if (r->noise > s->maxNoise) s->maxNoise = r->noise;
        if (r->ph < s->minPh) s->minPh = r->ph;
        if (r->ph > s->maxPh) s->maxPh = r->ph;
        if (mask) s->flagged++;
        if (mask & FLAG_AQI) s->flagAqi++;
        if (mask & FLAG_PH) s->flagPh++;
        if (mask & FLAG_NOISE) s->flagNoise++;
    }
    s->avgAqi = sumAqi / (double)db->count;
    s->avgPh = sumPh / (double)db->count;
    s->avgNoise = sumNoise / (double)db->count;
    s->locations = countUniqueLocations(db);
}

static void displaySummary(void)
{
    Stats s;
    printf("\n--- Pollution Summary Dashboard ---\n");
    computeStats(&g_db, &s);
    if (s.total == 0) {
        printf("  No records available. Nothing to summarise.\n");
        return;
    }
    printf("  +--------------------------------+------------------+\n");
    printf("  | Total records                  | %16lu |\n", (unsigned long)s.total);
    printf("  | Number of locations            | %16lu |\n", (unsigned long)s.locations);
    printf("  | Average AQI                    | %16.1f |\n", s.avgAqi);
    printf("  | Average water pH               | %16.2f |\n", s.avgPh);
    printf("  | Average noise (dB)             | %16.1f |\n", s.avgNoise);
    printf("  | Highest AQI                    | %16d |\n", s.maxAqi);
    printf("  | Highest noise (dB)             | %16.1f |\n", s.maxNoise);
    printf("  | Flagged records                | %16lu |\n", (unsigned long)s.flagged);
    printf("  +--------------------------------+------------------+\n");
    printf("  Note: averages over different dates and places can hide important local\n"
           "  conditions. Always look at individual readings too.\n");
}

static void writeLocationSummary(FILE *out)
{
    size_t i, k;
    for (i = 0; i < g_db.count; i++) {
        size_t n = 0, flagged = 0;
        double sa = 0, sp = 0, sn = 0;
        int maxA = 0;
        if (!isFirstOccurrence(&g_db, i)) {
            continue;
        }
        for (k = 0; k < g_db.count; k++) {
            const Record *r = &g_db.items[k];
            if (equalsIgnoreCase(r->location, g_db.items[i].location)) {
                if (n == 0 || r->aqi > maxA) maxA = r->aqi;
                n++;
                sa += r->aqi;
                sp += r->ph;
                sn += r->noise;
                if (flagMask(r)) flagged++;
            }
        }
        fprintf(out, "  %-24.24s readings=%-3lu avgAQI=%-6.1f maxAQI=%-4d avgpH=%-5.2f avgdB=%-6.1f flagged=%lu\n",
                g_db.items[i].location, (unsigned long)n, sa / (double)n, maxA,
                sp / (double)n, sn / (double)n, (unsigned long)flagged);
    }
}

static int generateReport(void)
{
    FILE *fp;
    Stats s;
    char stamp[64] = "not available";
    time_t now = time(NULL);
    struct tm *lt;
    int changes;

    printf("\n--- Generate Pollution Report ---\n");
    fp = fopen(REPORT_FILE, "w");
    if (fp == NULL) {
        printf("  Error: cannot create %s (%s).\n", REPORT_FILE, strerror(errno));
        return 0;
    }
    lt = localtime(&now);
    if (lt != NULL) {
        strftime(stamp, sizeof stamp, "%Y-%m-%d %H:%M:%S", lt);
    }
    computeStats(&g_db, &s);

    fprintf(fp, "==============================================================\n");
    fprintf(fp, " EARTHGUARD - POLLUTION PATTERN DETECTOR : POLLUTION REPORT\n");
    fprintf(fp, "==============================================================\n");
    fprintf(fp, "Generated on : %s\n", stamp);
    fprintf(fp, "Data source  : %s (values entered manually by the user)\n\n", DATA_FILE);

    fprintf(fp, "1. OVERALL SUMMARY\n");
    fprintf(fp, "  Total records     : %lu\n", (unsigned long)s.total);
    fprintf(fp, "  Unique locations  : %lu\n", (unsigned long)s.locations);
    if (s.total == 0) {
        fprintf(fp, "  No records are available, so no statistics were calculated.\n");
    } else {
        fprintf(fp, "  Average AQI       : %.1f\n", s.avgAqi);
        fprintf(fp, "  Average pH        : %.2f\n", s.avgPh);
        fprintf(fp, "  Average noise     : %.1f dB\n", s.avgNoise);
        fprintf(fp, "  Highest AQI       : %d\n", s.maxAqi);
        fprintf(fp, "  Highest noise     : %.1f dB\n", s.maxNoise);
        fprintf(fp, "  Lowest pH         : %.2f\n", s.minPh);
        fprintf(fp, "  Highest pH        : %.2f\n", s.maxPh);
    }

    fprintf(fp, "\n2. RECORDS FLAGGED BY RULE\n");
    {
        char label[64];
        snprintf(label, sizeof label, "AQI above %d", AQI_FLAG_ABOVE);
        fprintf(fp, "  %-30s: %lu\n", label, (unsigned long)s.flagAqi);
        snprintf(label, sizeof label, "pH outside %.1f-%.1f", PH_SCREEN_MIN, PH_SCREEN_MAX);
        fprintf(fp, "  %-30s: %lu\n", label, (unsigned long)s.flagPh);
        snprintf(label, sizeof label, "Noise above %.0f dB", NOISE_FLAG_ABOVE);
        fprintf(fp, "  %-30s: %lu\n", label, (unsigned long)s.flagNoise);
    }
    fprintf(fp, "  %-30s: %lu\n", "Records with at least one flag", (unsigned long)s.flagged);

    fprintf(fp, "\n3. LOCATION-WISE SUMMARY\n");
    if (s.total == 0) {
        fprintf(fp, "  (no data)\n");
    } else {
        writeLocationSummary(fp);
    }

    fprintf(fp, "\n4. UNUSUAL CHANGES (rules: AQI >= %d, noise >= %.0f dB, pH >= %.1f)\n",
            AQI_CHANGE_THRESHOLD, NOISE_CHANGE_THRESHOLD, PH_CHANGE_THRESHOLD);
    if (s.total < 2) {
        fprintf(fp, "  Not enough historical data to detect changes.\n");
    } else {
        changes = analyzeChanges(fp);
        if (changes >= 0) {
            fprintf(fp, "\n  Flagged changes: %d\n", changes);
        }
    }

    fprintf(fp, "\n5. RECOMMENDATIONS\n");
    fprintf(fp, "  - Verify flagged readings with calibrated instruments.\n");
    fprintf(fp, "  - Compare with the official standard that applies to your region.\n");
    fprintf(fp, "  - Keep recording readings regularly so trends can be reviewed.\n");

    fprintf(fp, "\n6. LIMITATIONS\n");
    fprintf(fp, "  - AQI categories are an illustrative project scale, not the official AQI.\n");
    fprintf(fp, "  - pH is only one water-quality indicator; it cannot prove water is safe.\n");
    fprintf(fp, "  - Noise impact depends on duration, environment and the applicable standard.\n");
    fprintf(fp, "  - Averages across places and dates can hide local conditions.\n");
    fprintf(fp, "  - Change flags are review rules, not confirmed pollution events.\n");

    fprintf(fp, "\nDISCLAIMER: EarthGuard analyses data entered by the user. It does not\n");
    fprintf(fp, "measure pollution and is not a certified monitoring instrument.\n");

    if (ferror(fp)) {
        printf("  Error: a write error occurred while creating the report.\n");
        fclose(fp);
        return 0;
    }
    if (fclose(fp) != 0) {
        printf("  Error: could not finish writing %s.\n", REPORT_FILE);
        return 0;
    }
    printf("  Success: report written to %s.\n", REPORT_FILE);
    return 1;
}

/* ------------------------------------------------------------------ */
/* Menu 10: Project information                                        */
/* ------------------------------------------------------------------ */
static void displayProjectInfo(void)
{
    printf("\n--- Project Information ---\n");
    printf("  Name     : %s\n", APP_NAME);
    printf("  Version  : %s\n", APP_VERSION);
    printf("  Language : Standard C (C11), compiled with GCC\n");
    printf("  Purpose  : Store, validate, classify and compare manually entered\n"
           "             AQI, water pH and noise readings, and find unusual changes.\n");
    printf("  Features : add/view/analyze readings, compare locations, change detection,\n"
           "             search, sort, text report, summary dashboard.\n");
    printf("  Storage  : text file %s (format ID|Location|Date|AQI|pH|dB);\n"
           "             report in %s.\n", DATA_FILE, REPORT_FILE);
    printf("  Limits   : max %d records; AQI scale and thresholds are project\n"
           "             assumptions, not official standards; it does not measure\n"
           "             pollution; pH alone cannot show water is safe; no medical,\n"
           "             legal or compliance conclusions are made.\n", MAX_RECORDS);
}

/* ------------------------------------------------------------------ */
/* main                                                                */
/* ------------------------------------------------------------------ */
int main(void)
{
    LoadInfo info;
    char buf[INPUT_BUF];
    int running = 1;

    displayHeader();
    if (!loadRecords(DATA_FILE, &g_db, &info)) {
        printf("  The data file could not be read. Existing data was NOT modified.\n");
        printf("  You can continue, but saved records will not be shown until the problem is fixed.\n");
    } else if (info.fileMissing) {
        printf("  No data file yet. It will be created when you add the first reading.\n");
    } else {
        printf("  Loaded %lu record(s) from %s.\n", (unsigned long)g_db.count, DATA_FILE);
    }
    reportLoadInfo(&info);

    while (running) {
        long choice;
        displayMenu();
        promptLine("Enter your choice (1-11): ", buf, sizeof buf, 0);
        if (!parseLongStrict(buf, &choice)) {
            printf("  Invalid option. Please type a number from 1 to 11.\n");
            continue;
        }
        switch (choice) {
        case 1:  addReading();         pressEnter(); break;
        case 2:  viewRecords();        pressEnter(); break;
        case 3:  analyzeMenu();        pressEnter(); break;
        case 4:  compareLocations();   pressEnter(); break;
        case 5:  detectChanges();      pressEnter(); break;
        case 6:  searchRecords();      pressEnter(); break;
        case 7:  sortRecords();        pressEnter(); break;
        case 8:  generateReport();     pressEnter(); break;
        case 9:  displaySummary();     pressEnter(); break;
        case 10: displayProjectInfo(); pressEnter(); break;
        case 11: running = 0; break;
        default:
            printf("  Invalid option. Please choose a number from 1 to 11.\n");
            break;
        }
    }

    freeResources(&g_db);
    printf("\nThank you for using EarthGuard. All saved records are kept in %s.\n", DATA_FILE);
    printf("Stay curious, stay green.\n\n");
    return 0;
}
