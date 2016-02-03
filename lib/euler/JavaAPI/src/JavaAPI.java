import java.lang.Class;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

public class JavaAPI {
  public static void main(String[] args) {
    try {
      Class setClass = Class.forName("Set" + args[0]);

      Method problemMethod = setClass.getDeclaredMethod("problem" + args[1]);

      problemMethod.invoke(null);

    } catch(ClassNotFoundException | NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {

      e.printStackTrace();

    }
  }
}
