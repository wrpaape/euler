JFLAGS = -g -Xlint
# JFLAGS = -g -Xlint -Xdiags:verbose

JC = javac

.SUFFIXES: .java .class

.java.class:
	$(JC) $(JFLAGS) $*.java -d ../

CLASSES = JavaAPI.java Set2.java Set3.java Set4.java

default: classes

classes: $(CLASSES:.java=.class)

clean:
	$(RM) ../*.class
