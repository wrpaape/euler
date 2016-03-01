/************************************************************************************
 *                                 - JavaAPI.java -                                 *
 *                                                                                  *
 * Houses the main class of the Java API, responsible for communication between     *
 * Elixir Mix project 'euler' and problems solved in Java.                          *
 ************************************************************************************/
import java.lang.Class;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

public class JavaAPI {
  private Method problemMethod; // method corresponding to requested problem number
  private Object solution;      // problem solution returned by 'problemMethod'
  private long timeElapsed;     // time (μs) taken to solve requested problem number

  private JavaAPI(String setNum, String probNum) {
    try {
      Class<?> problemSet = Class.forName("Set" + setNum);
      problemMethod		  = problemSet.getDeclaredMethod("problem" + probNum);

    } catch(ClassNotFoundException e) {
      exitOnError("Problem set class 'Set"
                + setNum
                + "' housing requested problem number '"
                + probNum
                + "' does not exist");

    } catch(NoSuchMethodException e) {
      exitOnError("Problem method 'problem'"
                + probNum
                + "' was not found in requested problem set class 'Set"
                + setNum
                + "'");
    }
  }

  private void solveProblem() {
    try {
      long timeStart = System.nanoTime();              // start clock
      solution       = problemMethod.invoke(null);     // solve problem
      long timeStop  = System.nanoTime();              // stop clock
      timeElapsed    = (timeStop - timeStart) / 1_000; // convert diff ns → μs

    } catch(IllegalAccessException e) {
      exitOnError("JavaAPI method 'solveProblem' does not have access to '"
                + problemMethod.toString()
                + "'");

    } catch(InvocationTargetException e) {
      exitOnError("'"
                + e.getTargetException()
                + "' thrown calling problem method '"
                + problemMethod.toString()
                + "'");
    }
  }

  private void reportSolution() {
    System.out.format(solution.toString() + "\n%d", timeElapsed);
  }

  private void exitOnError(String message) {
    System.out.print("\nERROR:\n  " + message); // print error message to stderr
    System.exit(1);                             // exit with status '1'
  }

  public static void main(String[] args) {
    JavaAPI api = new JavaAPI(args[0], args[1]);

    api.solveProblem();

    api.reportSolution();
  }
}
