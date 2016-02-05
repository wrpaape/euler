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
    private Class problemSet;
    private Method problemMethod;
    private Object solution;
    private long timeStart;
    private long timeStop;
    private long timeElapsed;

  private JavaAPI(String setNum, String probNum) {
    try {
    problemSet    = Class.forName("Set" + setNum);
    problemMethod = problemSet.getDeclaredMethod("problem" + probNum);
    } catch(ClassNotFoundException e) {
      e.printStackTrace();
    } catch(NoSuchMethodException e) {
      e.printStackTrace();
    }
    // private Class problemSet     = Class.forName("Set" + setNum);
    // private Method problemMethod = setClass.getDeclaredMethod("problem" + probNum);
    // private Object solution;
    // private long timeStart;
    // private long timeStop;
    // private long timeElapsed;
  };

  private void solveProblem() {
    try {
      timeStart = System.nanoTime();
      solution  = problemMethod.invoke(null);
      timeStop  = System.nanoTime();

      timeElapsed = (timeStop - timeStart) / 1_000; // convert ns → μs
    } catch(IllegalAccessException e) {
      e.printStackTrace();
    } catch(InvocationTargetException e) {
      e.printStackTrace();
    }
  }

  private void reportSolution() {
    System.out.format(solution.toString() + "\n%d", timeElapsed);
  }

  public static void main(String[] args) {
    // try {
      JavaAPI api = new JavaAPI(args[0], args[1]);

      api.solveProblem();

      api.reportSolution();
      // Class setClass = Class.forName("Set" + args[0]);

      // Method problemMethod = setClass.getDeclaredMethod("problem" + args[1]);

      // long timeStart = System.nanoTime();

      // problemMethod.invoke(null);

      // long timeStop = System.nanoTime();

      // long timeElapsed = (timeStop - timeStart) / 1_000; // convert ns → μs

      // System.out.format("\n%d", timeElapsed);

    // } catch(ClassNotFoundException e) {
    //   e.printStackTrace();

    // } catch(NoSuchMethodException e) {
    //   e.printStackTrace();

    // } catch(IllegalAccessException e) {
    //   e.printStackTrace();

    // } catch(InvocationTargetException e) {
    //   e.printStackTrace();

    // }
  }
}
