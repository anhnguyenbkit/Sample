

/**
 * @author nhphung
 */
package mc.parser
import org.antlr.v4.runtime.BaseErrorListener
import java.io.PrintWriter
import org.antlr.v4.runtime.RecognitionException
import org.antlr.v4.runtime.Recognizer;

trait ProcessError {
    def createRecoverErrorListener(dev:PrintWriter) = new BaseErrorListener() {
          override  def syntaxError(arg0:Recognizer[_, _] , obj: Any, line:Int, position:Int,message:String, ex:RecognitionException ) {
              dev.println(s"Error on line $line col ${position + 1}");
            }           
        }
    def createErrorListener() = new BaseErrorListener() {
          override  def syntaxError(arg0:Recognizer[_, _] , obj: Any, line:Int, position:Int,message:String, ex:RecognitionException ) {
              throw new IllegalArgumentException(s"Error on line $line col ${position + 1}");
            }           
        }
}