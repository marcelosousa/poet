/*
 * closelog.c
 */

#include <syslog.h>
#include <unistd.h>

// Cesar: external, to avoid inlining
#if 0
extern int __syslog_fd;

void closelog(void)
{
	int logfd = __syslog_fd;

	if (logfd != -1) {
		close(logfd);
		__syslog_fd = -1;
	}
}
#endif
