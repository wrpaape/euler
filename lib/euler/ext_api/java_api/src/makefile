JFLAGS = -g

JC = javac

.SUFFIXES: .java .class

.java.class:
				$(JC) $(JFLAGS) $*.java -d ../

CLASSES = JavaAPI.java Set2.java

default: classes

classes: $(CLASSES:.java=.class)

clean:
				$(RM) ../*.class
