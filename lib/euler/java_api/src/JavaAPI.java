import java.lang.Class;

public class JavaAPI {
  public static void main(String[] args) {
    try {
      Class<?> setClass = Class.forName("Set" + args[0]);

      setClass.dispatch(args[1]);

    } catch(ClassNotFoundException e){
      e.printStackTrace();
    }
  }
}
