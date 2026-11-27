import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Locale;
import java.util.stream.Stream;

import com.joanju.proparse.Environment;
import org.prorefactor.core.schema.Schema;
import org.prorefactor.treeparser.ParseUnit;

/**
 * Parses every compile unit (.p .w .cls .v) under a root with the
 * Consultingwerk Proparse, the way its README describes a host: Environment
 * settings, Schema.loadSchema, then one ParseUnit per file and parse().
 * The library keeps its settings in process-wide singletons, so it runs on
 * one thread. Prints the time of each pass over the tree.
 */
public class CwBench {
  public static void main(String[] args) throws Exception {
    Path root = null;
    String propath = "";
    String schema = null;
    String charset = "UTF-8";
    int passes = 3;
    for (int i = 0; i < args.length; i += 2) {
      switch (args[i]) {
        case "--root" -> root = Path.of(args[i + 1]);
        case "--propath" -> propath = args[i + 1];
        case "--schema" -> schema = args[i + 1];
        case "--charset" -> charset = args[i + 1];
        case "--passes" -> passes = Integer.parseInt(args[i + 1]);
        default -> throw new IllegalArgumentException(args[i]);
      }
    }
    long t0 = System.nanoTime();
    Environment env = Environment.instance();
    env.configSet("propath", propath);
    env.configSet("opsys", "unix");
    env.configSet("proversion", "12.8");
    env.configSet("batch-mode", "true");
    env.configSet("window-system", "TTY");
    if (schema != null)
      Schema.getInstance().loadSchema(schema);

    List<Path> files;
    try (Stream<Path> s = Files.walk(root)) {
      files = s.filter(Files::isRegularFile).filter(p -> {
        String n = p.getFileName().toString().toLowerCase(Locale.ROOT);
        return n.endsWith(".p") || n.endsWith(".w") || n.endsWith(".cls") || n.endsWith(".v");
      }).sorted().toList();
    }
    System.err.printf("files=%d setup_ms=%.1f%n", files.size(), (System.nanoTime() - t0) / 1e6);

    for (int pass = 1; pass <= passes; pass++) {
      int failures = 0;
      long start = System.nanoTime();
      for (Path f : files) {
        try {
          ParseUnit unit = new ParseUnit(new File(f.toString()), charset);
          unit.parse();
        } catch (Throwable e) {
          failures++;
          if (pass == 1 && failures <= 5) {
            String m = String.valueOf(e.getMessage()).replace('\n', ' ');
            System.err.println("FAIL " + f + ": " + e.getClass().getSimpleName() + " "
                + m.substring(0, Math.min(200, m.length())));
          }
        }
      }
      double ms = (System.nanoTime() - start) / 1e6;
      System.out.printf("{\"pass\":%d,\"ms\":%.1f,\"files\":%d,\"failures\":%d,\"threads\":1}%n",
          pass, ms, files.size(), failures);
    }
  }
}
