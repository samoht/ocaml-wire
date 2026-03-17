/**
 * CLCW polling loop benchmark (Java).
 *
 * Mirrors bench/bench_clcw.ml: simulates a COP-1 receiver polling CLCW words
 * at frame rate.  For each 4-byte word we read Lockout, Wait, Retransmit flags
 * (1-bit bitfields) and ReportValue (8-bit), compare with expected sequence,
 * and flag anomalies.
 *
 * Two implementations:
 *   1. Hand-written Java (shift/mask on a contiguous byte[] -- like the C baseline)
 *   2. dariol83/ccsds  Clcw class  (allocates a Clcw object per word)
 *
 * CLCW bit layout (CCSDS 232.0-B-3, 32 bits big-endian):
 *   bit 31      : ControlWordType (0)
 *   bits 30-29  : Version (2)
 *   bits 28-26  : StatusField (3)
 *   bits 25-24  : COPInEffect (2)
 *   bits 23-18  : VCID (6)
 *   bits 17-16  : Spare (2)
 *   bit 15      : NoRF (1)
 *   bit 14      : NoBitlock (1)
 *   bit 13      : Lockout (1)
 *   bit 12      : Wait (1)
 *   bit 11      : Retransmit (1)
 *   bits 10-9   : FARMBCounter (2)
 *   bit 8       : (spare)
 *   bits 7-0    : ReportValue (8)
 */

// TODO: uncomment the dariol83 import once the jar is on the classpath
// import eu.dariolucia.ccsds.tmtc.ocf.pdu.Clcw;

public class BenchClcw {

    static final int N_WORDS  = 10_000_000;
    static final int N_ROUNDS = 10;
    static final int WORD_SIZE = 4;

    // ── Default CLCW template (matches Space.clcw_default in OCaml) ──
    // type=0, version=0, status=0, cop=1, vcid=7, spare=0,
    // no_rf=0, no_bitlock=0, lockout=0, wait=0, retransmit=0, farmb=0, report=42
    static int clcwDefault() {
        int w = 0;
        // cop=1  -> bit 24
        w |= (1 << 24);
        // vcid=7 -> bits 23-18
        w |= (7 << 18);
        // report=42 -> bits 7-0
        w |= 42;
        return w;
    }

    /**
     * Generate a contiguous byte[] of N packed CLCW words.
     * Each word: lockout=0, wait=0, retransmit=0, report=(i % 256).
     * Matches the OCaml generate_stream function.
     */
    static byte[] generateStream(int n) {
        byte[] buf = new byte[n * WORD_SIZE];
        int template = clcwDefault();

        for (int i = 0; i < n; i++) {
            // Clear lockout (bit 13), wait (bit 12), retransmit (bit 11),
            // report (bits 7-0), then set report = i % 256.
            int w = template & ~(0x00003800 | 0xFF);  // clear flags + report
            w |= (i & 0xFF);                          // set report

            int off = i * WORD_SIZE;
            buf[off]     = (byte) (w >>> 24);
            buf[off + 1] = (byte) (w >>> 16);
            buf[off + 2] = (byte) (w >>> 8);
            buf[off + 3] = (byte) w;
        }
        return buf;
    }

    // ── 1. Hand-written Java: shift/mask on contiguous byte[] ──

    static long benchHandWritten(byte[] buf, int nWords, int[] outAnomalies) {
        int anomalies = 0;
        int expectedSeq = 0;

        long t0 = System.nanoTime();
        for (int i = 0; i < nWords; i++) {
            int off = i * WORD_SIZE;
            byte b2 = buf[off + 2];
            byte b3 = buf[off + 3];

            int lockout    = (b2 >> 5) & 1;
            int wait       = (b2 >> 4) & 1;
            int retransmit = (b2 >> 3) & 1;
            int report     = b3 & 0xFF;

            if (lockout != 0 || wait != 0 || retransmit != 0
                    || report != (expectedSeq & 0xFF)) {
                anomalies++;
            }
            expectedSeq = report;
        }
        long dt = System.nanoTime() - t0;

        outAnomalies[0] = anomalies;
        return dt;
    }

    // ── 2. dariol83/ccsds Clcw: allocates per word ──

    /**
     * Uses eu.dariolucia.ccsds.tmtc.ocf.pdu.Clcw to parse each 4-byte word.
     * The Clcw constructor validates the word and extracts all fields.
     *
     * NOTE: This allocates a new Clcw object + a 4-byte array slice per word,
     * so it measures library overhead (object creation + validation + GC).
     */
    static long benchDariol83(byte[] buf, int nWords, int[] outAnomalies) {
        // TODO: uncomment once dariol83 jar is on the classpath.
        //
        // int anomalies = 0;
        // int expectedSeq = 0;
        // byte[] word = new byte[4];
        //
        // long t0 = System.nanoTime();
        // for (int i = 0; i < nWords; i++) {
        //     int off = i * WORD_SIZE;
        //     System.arraycopy(buf, off, word, 0, 4);
        //     Clcw clcw = new Clcw(word);
        //
        //     int lockout    = clcw.isLockoutFlag()    ? 1 : 0;
        //     int wait       = clcw.isWaitFlag()        ? 1 : 0;
        //     int retransmit = clcw.isRetransmitFlag()  ? 1 : 0;
        //     int report     = clcw.getReportValue();
        //
        //     if (lockout != 0 || wait != 0 || retransmit != 0
        //             || report != (expectedSeq & 0xFF)) {
        //         anomalies++;
        //     }
        //     expectedSeq = report;
        // }
        // long dt = System.nanoTime() - t0;
        //
        // outAnomalies[0] = anomalies;
        // return dt;

        outAnomalies[0] = -1;
        return -1;
    }

    // ── 3. dariol83/ccsds with array reuse (avoids per-word allocation) ──

    /**
     * Optimised dariol83 variant: reuses the same 4-byte array for each word.
     * The Clcw constructor does NOT copy its input (documented behaviour), so
     * we can overwrite-in-place and re-parse.  This still allocates a Clcw
     * object per word but avoids the byte[] copy.
     */
    static long benchDariol83Reuse(byte[] buf, int nWords, int[] outAnomalies) {
        // TODO: uncomment once dariol83 jar is on the classpath.
        //
        // int anomalies = 0;
        // int expectedSeq = 0;
        // byte[] word = new byte[4];
        //
        // long t0 = System.nanoTime();
        // for (int i = 0; i < nWords; i++) {
        //     int off = i * WORD_SIZE;
        //     word[0] = buf[off];
        //     word[1] = buf[off + 1];
        //     word[2] = buf[off + 2];
        //     word[3] = buf[off + 3];
        //     Clcw clcw = new Clcw(word);
        //
        //     int lockout    = clcw.isLockoutFlag()    ? 1 : 0;
        //     int wait       = clcw.isWaitFlag()        ? 1 : 0;
        //     int retransmit = clcw.isRetransmitFlag()  ? 1 : 0;
        //     int report     = clcw.getReportValue();
        //
        //     if (lockout != 0 || wait != 0 || retransmit != 0
        //             || report != (expectedSeq & 0xFF)) {
        //         anomalies++;
        //     }
        //     expectedSeq = report;
        // }
        // long dt = System.nanoTime() - t0;
        //
        // outAnomalies[0] = anomalies;
        // return dt;

        outAnomalies[0] = -1;
        return -1;
    }

    // ── Main ──

    public static void main(String[] args) {
        byte[] buf = generateStream(N_WORDS);
        long totalOps = (long) N_WORDS * N_ROUNDS;

        System.out.printf("CLCW polling loop (%d words, %dB each, contiguous buffer)%n%n",
                N_WORDS, WORD_SIZE);

        // ── Hand-written Java ──
        int[] hwAnomalies = { 0 };
        long hwTotalNs = 0;
        for (int r = 0; r < N_ROUNDS; r++) {
            hwTotalNs += benchHandWritten(buf, N_WORDS, hwAnomalies);
        }
        double hwNsPer = (double) hwTotalNs / totalOps;
        double hwMchecks = totalOps / (hwTotalNs / 1e9) / 1e6;

        System.out.printf("  %-28s %6.1f ns/word  %5.1f Mcheck/s  (%d anomalies)%n",
                "Java (hand-written)", hwNsPer, hwMchecks, hwAnomalies[0]);

        // ── dariol83/ccsds ──
        int[] d83Anomalies = { 0 };
        long d83TotalNs = 0;
        for (int r = 0; r < N_ROUNDS; r++) {
            d83TotalNs += benchDariol83(buf, N_WORDS, d83Anomalies);
        }
        if (d83Anomalies[0] >= 0) {
            double d83NsPer = (double) d83TotalNs / totalOps;
            double d83Mchecks = totalOps / (d83TotalNs / 1e9) / 1e6;
            double d83Ratio = d83NsPer / hwNsPer;
            System.out.printf("  %-28s %6.1f ns/word  %5.1f Mcheck/s  (%d anomalies)  (%.1fx)%n",
                    "dariol83/ccsds Clcw", d83NsPer, d83Mchecks, d83Anomalies[0], d83Ratio);
        } else {
            System.out.printf("  %-28s   (skipped -- uncomment TODO to enable)%n",
                    "dariol83/ccsds Clcw");
        }

        // ── dariol83/ccsds (array reuse) ──
        int[] d83rAnomalies = { 0 };
        long d83rTotalNs = 0;
        for (int r = 0; r < N_ROUNDS; r++) {
            d83rTotalNs += benchDariol83Reuse(buf, N_WORDS, d83rAnomalies);
        }
        if (d83rAnomalies[0] >= 0) {
            double d83rNsPer = (double) d83rTotalNs / totalOps;
            double d83rMchecks = totalOps / (d83rTotalNs / 1e9) / 1e6;
            double d83rRatio = d83rNsPer / hwNsPer;
            System.out.printf("  %-28s %6.1f ns/word  %5.1f Mcheck/s  (%d anomalies)  (%.1fx)%n",
                    "dariol83/ccsds (reuse)", d83rNsPer, d83rMchecks, d83rAnomalies[0], d83rRatio);
        } else {
            System.out.printf("  %-28s   (skipped -- uncomment TODO to enable)%n",
                    "dariol83/ccsds (reuse)");
        }

        // ── Anomaly cross-check ──
        System.out.println();
        if (d83Anomalies[0] >= 0 && hwAnomalies[0] != d83Anomalies[0]) {
            System.out.printf("  MISMATCH! hand-written: %d anomalies, dariol83: %d anomalies%n",
                    hwAnomalies[0], d83Anomalies[0]);
        } else {
            System.out.printf("  %d anomalies detected (hand-written baseline)%n", hwAnomalies[0]);
        }
    }
}
