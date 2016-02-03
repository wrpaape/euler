import java.lang.Class;
import java.lang.reflect.Method;

public class Set2 {

  public static void dispatch(String problemNumber) { 
    try {
      Set2 setObj = new Set2();

      Class<?> setClass = setObj.getClass();

      Method problemMethod = setClass.getMethod("problem" + problemNumber);

      problemMethod.invoke(setObj);

    } catch(NoSuchMethodException e){
      e.printStackTrace();
    }
  }

  public static void problem15() { 

    System.out.println("YO");

  }

}