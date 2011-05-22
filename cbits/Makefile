CXXFLAGS = -Wall $(shell xapian-config --cxxflags)
LDFLAGS = $(shell xapian-config --libs) -shared

OBJ = $(shell echo *.cc | sed 's/\.cc/\.o/g')

all: libcxapian.so

clean: 
	rm -f $(OBJ)
	rm -f libcxapian.so

libcxapian.so: $(OBJ)
	$(CXX) $(CXXFLAGS) -o libcxapian.so $(OBJ) $(LDFLAGS)

.PHONY: all clean