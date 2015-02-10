all:
	erl -make
	erl -sname gateway -pa ebin -s gateway

clean:
	rm -rf ebin/*.beam

