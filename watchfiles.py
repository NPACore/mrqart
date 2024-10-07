#!/usr/bin/env python
"""
want for new files and act on them
1. check protocol against DB
2. run alignment and report
3. send websockets message to server/browser
"""

from twisted.internet import inotify
from twisted.python import filepath
from twisted.internet import reactor


def on_notify(ignored, filepath, mask):
    print(filepath)


if __name__ == '__main__':
    watch_dir = 'sim/' # NB. sim/out has new files but directory must exist
    watch_dir = filepath.FilePath(watch_dir)
    print(watch_dir)
    notifier = inotify.INotify()
    notifier.startReading()
    notifier.watch(watch_dir, callbacks=[on_notify])

    reactor.run()
