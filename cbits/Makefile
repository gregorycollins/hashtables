check: default.c common.c check.c sse-42.c
	@echo "Testing portable version..."
	@gcc -o check -O3 default.c common.c check.c
	@./check
	@rm check

	@echo
	@echo "Testing SSE 4.2 version..."
	@gcc -o check -O3 -msse4.2 sse-42.c common.c check.c sse-42-check.c
	@./check
	@rm check

clean:
	rm -f *.o check
