@rem   FIRMM DICOM streaming start
@rem   IceConfig commands courtesy of jonathan polimeni <jonp@nmr.mgh.harvard.edu>
@rem   for Prisma VE11B and VE11C

@echo off

rem set window size for ASCII art
mode con: cols=100 lines=50

rem specify settings
@SET DICOM_PATH=\\10.48.88.92\dicomstream
@SET FIRMM_USER=mrqart
@SET FIRMM_SAMBA_PASS=DependMagnetBit

rem delete mount if it already exists, mount drive
net use %DICOM_PATH% /Delete
net use %DICOM_PATH% /user:%FIRMM_USER% %FIRMM_SAMBA_PASS%

@echo on

xedit -f C:\MedCom\config\Ice\IceConfig.evp -n ICE.CONFIG.OnlineSendConfiguration -p OnlineSendIMA -v 1

xedit -f C:\MedCom\config\Ice\IceConfig.evp -n ICE.CONFIG.OnlineSendConfiguration -p OnlineTargetPort -v -1
xedit -f C:\MedCom\config\Ice\IceConfig.evp -n ICE.CONFIG.OnlineSendConfiguration -p OnlineTargetHostName -v localhost

xedit -f C:\MedCom\config\Ice\IceConfig.evp -n ICE.CONFIG.OnlineSendConfiguration -p OnlineTargetPath -v %DICOM_PATH%

@rem configure system such that the SendIMA value is reset to its default parameters after a "patreg" or when a new patient is registered
xedit -f C:\MedCom\config\Ice\IceConfig_source_patreg.evp -n ICE.CONFIG.OnlineSendConfiguration.OnlineSendIMA.SOURCE -p PATH -v C:\MedCom\config\Ice\IceConfig_orig.evp

@rem the default path is C:\MedCom\config\Ice\IceConfig.evp, which means that after a patreg the OnlineSendIMA value persists

@rem touch the host.txt file to refresh the mount and generate a time stamp when this script was last run
copy /y nul %DICOM_PATH%\host.txt
@SET COPY_STATUS=%ERRORLEVEL%
@echo off
if %COPY_STATUS%==0 (
	echo  #####                                                         
    echo #     #  ####  #    # #    # ######  ####  ##### ###### #####  
    echo #       #    # ##   # ##   # #      #    #   #   #      #    # 
    echo #       #    # # #  # # #  # #####  #        #   #####  #    # 
    echo #       #    # #  # # #  # # #      #        #   #      #    # 
    echo #     # #    # #   ## #   ## #      #    #   #   #      #    # 
    echo  #####   ####  #    # #    # ######  ####    #   ###### #####
) else (
    echo #     #                  #####                                                         
    echo ##    #  ####  #####    #     #  ####  #    # #    # ######  ####  ##### ###### #####  
    echo # #   # #    #   #      #       #    # ##   # ##   # #      #    #   #   #      #    # 
    echo #  #  # #    #   #      #       #    # # #  # # #  # #####  #        #   #####  #    # 
    echo #   # # #    #   #      #       #    # #  # # #  # # #      #        #   #      #    # 
    echo #    ## #    #   #      #     # #    # #   ## #   ## #      #    #   #   #      #    # 
    echo #     #  ####    #       #####   ####  #    # #    # ######  ####    #   ###### #####   
)

@rem wait 10 seconds to let user see window, then close automatically
timeout /t 10
