# re-generate

Run tests+benchmarks with

```
stack build --bench --test
```

## Benchmark Results

On 2,2 GHz Intel Core i7 (MacBook Pro (Retina, 15-inch, Mid 2014))

```
benchmarking generation for (a)*b with maxLen=Just 4 and complement alpha=ab/gen=Seg
time                 2.018 μs   (1.967 μs .. 2.083 μs)
                     0.995 R²   (0.994 R² .. 0.997 R²)
mean                 2.070 μs   (2.036 μs .. 2.101 μs)
std dev              105.3 ns   (89.76 ns .. 124.4 ns)
variance introduced by outliers: 65% (severely inflated)

benchmarking generation for ~((a)*b) with maxLen=Just 4 and complement alpha=ab/gen=Seg
time                 3.505 μs   (3.202 μs .. 3.841 μs)
                     0.965 R²   (0.944 R² .. 0.996 R²)
mean                 3.240 μs   (3.133 μs .. 3.422 μs)
std dev              432.7 ns   (240.2 ns .. 734.4 ns)
variance introduced by outliers: 93% (severely inflated)

benchmarking generation for ~((a)*b) with maxLen=Just 8 and complement alpha=ab/gen=Seg
time                 17.92 μs   (16.87 μs .. 19.47 μs)
                     0.956 R²   (0.927 R² .. 0.985 R²)
mean                 19.52 μs   (18.66 μs .. 20.69 μs)
std dev              3.413 μs   (2.468 μs .. 4.679 μs)
variance introduced by outliers: 95% (severely inflated)

benchmarking generation for ~((a)*b) with maxLen=Just 16 and complement alpha=ab/gen=Seg
time                 6.686 ms   (6.495 ms .. 6.884 ms)
                     0.995 R²   (0.991 R² .. 0.998 R²)
mean                 6.780 ms   (6.694 ms .. 6.861 ms)
std dev              238.0 μs   (187.0 μs .. 304.0 μs)
variance introduced by outliers: 16% (moderately inflated)
```
