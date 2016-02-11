JFLAGS = -g -Xlint

JC = javac

.SUFFIXES: .java .class

.java.class:
				$(JC) $(JFLAGS) $*.java -d ../

CLASSES = JavaAPI.java Set2.java Set3.java

default: classes

classes: $(CLASSES:.java=.class)

clean:
				$(RM) ../*.class
