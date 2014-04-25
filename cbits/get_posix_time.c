#include <stddef.h>
#include <math.h>
#include <sys/time.h>
#include <stdint.h>

// get POSIX time as FP value in seconds; returns NaN in case of error
double get_posix_time_secs(void)
{
  struct timeval tval = { 0, 0, };

  if (gettimeofday(&tval, NULL))
#ifdef NAN
    return (NAN);
#else
    return (0.0/0.0);
#endif

  return (((double) tval.tv_sec) + (((double) tval.tv_usec) / 1e6));
}

// returns number of microseconds since epoch; returns 0 in case of error
uint64_t get_posix_time_usecs(void)
{
  struct timeval tval = { 0, 0, };

  if (gettimeofday(&tval, NULL))
    return 0;

  return ((((uint64_t)tval.tv_sec) * 1000000) + ((uint64_t) tval.tv_usec));
}
