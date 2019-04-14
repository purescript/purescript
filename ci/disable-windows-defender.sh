#!/bin/bash

export NODEPATH=$(where.exe node.exe)
export PROJECTDIR=$(pwd)
export TEMPDIR=$LOCALAPPDATA\\Temp

powershell Add-MpPreference -ExclusionProcess ${NODEPATH}
powershell Add-MpPreference -ExclusionPath ${PROJECTDIR}
powershell Add-MpPreference -ExclusionPath ${TEMPDIR}

echo "DisableArchiveScanning..."
powershell Start-Process -PassThru -Wait PowerShell -ArgumentList "'-Command Set-MpPreference -DisableArchiveScanning \$true'"

echo "DisableBehaviorMonitoring..."
powershell Start-Process -PassThru -Wait PowerShell -ArgumentList "'-Command Set-MpPreference -DisableBehaviorMonitoring \$true'"

echo "DisableRealtimeMonitoring..."
powershell Start-Process -PassThru -Wait PowerShell -ArgumentList "'-Command Set-MpPreference -DisableRealtimeMonitoring \$true'"


