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
  public static void main(String[] args) {
    try {
      Class setClass = Class.forName("Set" + args[0]);

      Method problemMethod = setClass.getDeclaredMethod("problem" + args[1]);

      long timeStart = System.nanoTime();

      problemMethod.invoke(null);

      long timeStop = System.nanoTime();

      long timeElapsed = (timeStop - timeStart) / 1_000; // convert ns → μs

      System.out.format("\n%d", timeElapsed);

    } catch(ClassNotFoundException | NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {

      e.printStackTrace();

    }
  }
}
