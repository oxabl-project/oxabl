import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import eu.rssw.antlr.database.DumpFileUtils;
import eu.rssw.pct.mapping.OpenEdgeVersion;
import org.prorefactor.core.schema.IDatabase;
import org.prorefactor.core.schema.Schema;
import org.prorefactor.refactor.RefactorSession;
import org.prorefactor.refactor.settings.ProparseSettings;
import org.prorefactor.treeparser.ParseUnit;
import org.sonar.plugins.openedge.api.objects.DatabaseWrapper;

/**
 * Parses every compile unit (.p .w .cls .v) under a root with Proparse, as the
 * Riverside SonarQube plugin does: one ParseUnit per file, parse(), and with
 * --mode tree also treeParser01(). Prints the time of each pass over the tree.
 */
public class RiversideBench {
  public static void main(String[] args) throws Exception {
    Path root = null;
    String propath = "";
    List<Path> dfs = new ArrayList<>();
    int passes = 5;
    int threads = 1;
    String mode = "parse";
    Charset charset = Charset.forName("UTF-8");
    for (int i = 0; i < args.length; i += 2) {
      switch (args[i]) {
        case "--root" -> root = Path.of(args[i + 1]);
        case "--propath" -> propath = args[i + 1];
        case "--df" -> dfs.add(Path.of(args[i + 1]));
        case "--passes" -> passes = Integer.parseInt(args[i + 1]);
        case "--threads" -> threads = Integer.parseInt(args[i + 1]);
        case "--mode" -> mode = args[i + 1];
        case "--charset" -> charset = Charset.forName(args[i + 1]);
        default -> throw new IllegalArgumentException(args[i]);
      }
    }
    long jvmStart = java.lang.management.ManagementFactory.getRuntimeMXBean().getStartTime();
    long t0 = System.nanoTime();

    Schema schema;
    List<IDatabase> dbs = new ArrayList<>();
    for (Path df : dfs)
      dbs.add(new DatabaseWrapper(DumpFileUtils.getDatabaseDescription(df), OpenEdgeVersion.V128));
    schema = new Schema(dbs.toArray(new IDatabase[0]));
    final String pp = propath;
    final Charset cs = charset;
    final ThreadLocal<RefactorSession> session =
        ThreadLocal.withInitial(() -> new RefactorSession(new ProparseSettings(pp), schema, cs));

    List<Path> files;
    final Path base = root;
    try (Stream<Path> s = Files.walk(root)) {
      files = s.filter(Files::isRegularFile).filter(p -> {
        String n = p.getFileName().toString().toLowerCase(Locale.ROOT);
        return n.endsWith(".p") || n.endsWith(".w") || n.endsWith(".cls") || n.endsWith(".v");
      }).sorted().toList();
    }
    double setupMs = (System.nanoTime() - t0) / 1e6;
    System.err.printf("files=%d setup_ms=%.1f jvm_start_to_main_ms=%d%n", files.size(), setupMs,
        System.currentTimeMillis() - jvmStart - (long) setupMs);

    final boolean tree = mode.equals("tree");
    ExecutorService pool = Executors.newFixedThreadPool(threads);
    for (int pass = 1; pass <= passes; pass++) {
      AtomicInteger failures = new AtomicInteger();
      AtomicInteger syntax = new AtomicInteger();
      long start = System.nanoTime();
      List<Future<?>> jobs = new ArrayList<>();
      for (Path f : files) {
        jobs.add(pool.submit(() -> {
          try (InputStream in = Files.newInputStream(f)) {
            ParseUnit unit = new ParseUnit(in, base.relativize(f).toString(), session.get(), cs);
            unit.setQuiet(true);
            unit.parse();
            if (unit.hasSyntaxError())
              syntax.incrementAndGet();
            if (tree)
              unit.treeParser01();
          } catch (Throwable e) {
            failures.incrementAndGet();
            if (failures.get() <= 5)
              System.err.println("FAIL " + f + ": " + e.getClass().getSimpleName() + " " + e.getMessage());
          }
        }));
      }
      for (Future<?> j : jobs)
        j.get();
      double ms = (System.nanoTime() - start) / 1e6;
      System.out.printf("{\"pass\":%d,\"ms\":%.1f,\"files\":%d,\"failures\":%d,\"syntax_errors\":%d,\"mode\":\"%s\",\"threads\":%d}%n",
          pass, ms, files.size(), failures.get(), syntax.get(), mode, threads);
    }
    pool.shutdown();
  }
}
