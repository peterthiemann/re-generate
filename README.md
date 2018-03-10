# re-generate

Run tests+benchmarks with

```
stack build --bench --test
```

## Benchmark Results

On 2,2 GHz Intel Core i7 (MacBook Pro (Retina, 15-inch, Mid 2014))

```
benchmarking generation for (a)*b with maxLen=Just 4 and complement alpha=ab/gen=Seg
time                 4.565 μs   (4.467 μs .. 4.669 μs)
                     0.996 R²   (0.995 R² .. 0.998 R²)
mean                 4.515 μs   (4.443 μs .. 4.595 μs)
std dev              253.5 ns   (206.9 ns .. 328.7 ns)
variance introduced by outliers: 68% (severely inflated)

benchmarking generation for ~((a)*b) with maxLen=Just 4 and complement alpha=ab/gen=Seg
time                 7.163 μs   (7.022 μs .. 7.312 μs)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 7.213 μs   (7.117 μs .. 7.321 μs)
std dev              356.0 ns   (284.6 ns .. 477.4 ns)
variance introduced by outliers: 61% (severely inflated)

benchmarking generation for ~((a)*b) with maxLen=Just 8 and complement alpha=ab/gen=Seg
time                 48.77 μs   (47.80 μs .. 49.72 μs)
                     0.995 R²   (0.989 R² .. 0.998 R²)
mean                 48.97 μs   (48.07 μs .. 50.39 μs)
std dev              3.765 μs   (2.409 μs .. 6.929 μs)
variance introduced by outliers: 74% (severely inflated)

benchmarking generation for ~((a)*b) with maxLen=Just 16 and complement alpha=ab/gen=Seg
time                 14.82 ms   (14.32 ms .. 15.24 ms)
                     0.996 R²   (0.993 R² .. 0.999 R²)
mean                 14.89 ms   (14.69 ms .. 15.16 ms)
std dev              576.2 μs   (423.2 μs .. 863.0 μs)
variance introduced by outliers: 15% (moderately inflated)
```
