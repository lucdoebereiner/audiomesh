# Makefile for linux

# Adjust this path to point to include in sc sources
#SC_SRC_INCLUDE=~/Work/src/sc-310/supercollider/include
SC_SRC_INCLUDE=/usr/local/include/SuperCollider

# Optionally adjust installation path
SC_INSTALL_DIR=~/.local/share/SuperCollider/Extensions
#SC_INSTALL_DIR=/usr/local/share/SuperCollider/Extensions/

SC_PLUGIN_INTERFACE=${SC_SRC_INCLUDE}/plugin_interface
SC_COMMON=${SC_SRC_INCLUDE}/common
SC_SERVER=${SC_SRC_INCLUDE}/server

CC=g++
CPPFLAGS=-std=c++11 -Wno-deprecated -Wno-unknown-pragmas -D_REENTRANT -DNDEBUG -DSC_MEMORY_ALIGNMENT=1 -DSC_LINUX -Wno-deprecated -fPIC -O3

SRCS=wrap.cpp
OBJS=$(subst .cpp,.o,$(SRCS))


wrap: $(OBJS)
	$(CC) -shared -o wrap.so wrap.o ../target/release/libgraphugen.a


%.o: %.cpp 
	$(CC) $(CPPFLAGS) -I $(SC_PLUGIN_INTERFACE) -I $(SC_COMMON) -I $(SC_SERVER) -c $<


install: wrap.sc wrap.so
	cp wrap.sc $(SC_INSTALL_DIR)
	cp wrap.so $(SC_INSTALL_DIR)
