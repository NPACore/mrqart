@rem   FIRMM DICOM streaming stop
@rem   ideacmdtool line courtesy of jonathan polimeni <jonp@nmr.mgh.harvard.edu>

@rem [2] disable DICOM streaming
ideacmdtool 6

@rem wait 10 seconds to let user see window, then close automatically
@timeout /t 10
