#!/bin/bash

# Intended to speed up builds by disabling Windows Defender.
# See https://travis-ci.community/t/current-known-issues-please-read-this-before-posting-a-new-topic/264/15

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


