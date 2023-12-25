# pip install flask
from random import randint
from flask import Flask, request

APP = Flask(__name__)
A = lambda what: request.args.get(what)

@APP.route("/rest/system/ping")
def ping():
    return {
        "ping": "pong"
    }


@APP.route("/rest/config")
def config():
    return {
        "version": 37,
        "folders": [
            {
                "id": "folder-id-1",
                "label": "Notes",
                "filesystemType": "basic",
                "path": "~/notes",
                "type": "sendreceive",
                "devices": [
                    {
                        "deviceID": "device-id-1",
                        "introducedBy": "",
                        "encryptionPassword": ""
                    },
                    {
                        "deviceID": "device-current",
                        "introducedBy": "",
                        "encryptionPassword": ""
                    },
                    {
                        "deviceID": "device-id-3",
                        "introducedBy": "",
                        "encryptionPassword": ""
                    }
                ],
                "rescanIntervalS": 3600,
                "fsWatcherEnabled": True,
                "fsWatcherDelayS": 10,
                "ignorePerms": False,
                "autoNormalize": True,
                "minDiskFree": {
                    "value": 1,
                    "unit": "%"
                },
                "versioning": {
                    "type": "",
                    "params": {},
                    "cleanupIntervalS": 3600,
                    "fsPath": "",
                    "fsType": "basic"
                },
                "copiers": 0,
                "pullerMaxPendingKiB": 0,
                "hashers": 0,
                "order": "oldestFirst",
                "ignoreDelete": False,
                "scanProgressIntervalS": 0,
                "pullerPauseS": 0,
                "maxConflicts": 10,
                "disableSparseFiles": False,
                "disableTempIndexes": False,
                "paused": False,
                "weakHashThresholdPct": 25,
                "markerName": ".stfolder",
                "copyOwnershipFromParent": False,
                "modTimeWindowS": 0,
                "maxConcurrentWrites": 2,
                "disableFsync": False,
                "blockPullOrder": "standard",
                "copyRangeMethod": "standard",
                "caseSensitiveFS": False,
                "junctionsAsDirs": False,
                "syncOwnership": False,
                "sendOwnership": False,
                "syncXattrs": False,
                "sendXattrs": False,
                "xattrFilter": {
                    "entries": [],
                    "maxSingleEntrySize": 1024,
                    "maxTotalSize": 4096
                }
            },
            {
                "id": "folder-id-2",
                "label": "Photos",
                "filesystemType": "basic",
                "path": "/home/demo/photos",
                "type": "sendreceive",
                "devices": [
                    {
                        "deviceID": "device-id-1",
                        "introducedBy": "",
                        "encryptionPassword": ""
                    },
                    {
                        "deviceID": "device-current",
                        "introducedBy": "",
                        "encryptionPassword": ""
                    },
                    {
                        "deviceID": "device-id-2",
                        "introducedBy": "",
                        "encryptionPassword": ""
                    },
                    {
                        "deviceID": "device-id-3",
                        "introducedBy": "",
                        "encryptionPassword": ""
                    }
                ],
                "rescanIntervalS": 3600,
                "fsWatcherEnabled": True,
                "fsWatcherDelayS": 10,
                "ignorePerms": False,
                "autoNormalize": True,
                "minDiskFree": {
                    "value": 10,
                    "unit": "MB"
                },
                "versioning": {
                    "type": "",
                    "params": {},
                    "cleanupIntervalS": 3600,
                    "fsPath": "",
                    "fsType": "basic"
                },
                "copiers": 0,
                "pullerMaxPendingKiB": 0,
                "hashers": 0,
                "order": "smallestFirst",
                "ignoreDelete": False,
                "scanProgressIntervalS": 0,
                "pullerPauseS": 0,
                "maxConflicts": 10,
                "disableSparseFiles": False,
                "disableTempIndexes": False,
                "paused": False,
                "weakHashThresholdPct": 25,
                "markerName": ".stfolder",
                "copyOwnershipFromParent": False,
                "modTimeWindowS": 0,
                "maxConcurrentWrites": 2,
                "disableFsync": False,
                "blockPullOrder": "standard",
                "copyRangeMethod": "standard",
                "caseSensitiveFS": False,
                "junctionsAsDirs": False,
                "syncOwnership": False,
                "sendOwnership": False,
                "syncXattrs": False,
                "sendXattrs": False,
                "xattrFilter": {
                    "entries": [],
                    "maxSingleEntrySize": 0,
                    "maxTotalSize": 0
                }
            },
            {
                "id": "folder-id-3",
                "label": "Code",
                "filesystemType": "basic",
                "path": "/home/demo/code",
                "type": "sendreceive",
                "devices": [
                    {
                        "deviceID": "device-id-1",
                        "introducedBy": "",
                        "encryptionPassword": ""
                    },
                    {
                        "deviceID": "device-current",
                        "introducedBy": "",
                        "encryptionPassword": ""
                    },
                    {
                        "deviceID": "device-id-2",
                        "introducedBy": "",
                        "encryptionPassword": ""
                    },
                    {
                        "deviceID": "device-id-3",
                        "introducedBy": "",
                        "encryptionPassword": ""
                    }
                ],
                "rescanIntervalS": 3600,
                "fsWatcherEnabled": True,
                "fsWatcherDelayS": 10,
                "ignorePerms": False,
                "autoNormalize": True,
                "minDiskFree": {
                    "value": 1,
                    "unit": "%"
                },
                "versioning": {
                    "type": "",
                    "params": {},
                    "cleanupIntervalS": 3600,
                    "fsPath": "",
                    "fsType": "basic"
                },
                "copiers": 0,
                "pullerMaxPendingKiB": 0,
                "hashers": 0,
                "order": "random",
                "ignoreDelete": False,
                "scanProgressIntervalS": 0,
                "pullerPauseS": 0,
                "maxConflicts": 10,
                "disableSparseFiles": False,
                "disableTempIndexes": False,
                "paused": False,
                "weakHashThresholdPct": 25,
                "markerName": ".stfolder",
                "copyOwnershipFromParent": False,
                "modTimeWindowS": 0,
                "maxConcurrentWrites": 2,
                "disableFsync": False,
                "blockPullOrder": "standard",
                "copyRangeMethod": "standard",
                "caseSensitiveFS": False,
                "junctionsAsDirs": False,
                "syncOwnership": False,
                "sendOwnership": False,
                "syncXattrs": False,
                "sendXattrs": False,
                "xattrFilter": {
                    "entries": [],
                    "maxSingleEntrySize": 0,
                    "maxTotalSize": 0
                }
            }
        ],
        "devices": [
            {
                "deviceID": "device-id-1",
                "name": "Desktop",
                "addresses": [
                    "tcp://10.1.10.1:22000",
                    "tcp://192.168.0.4:22000",
                    "tcp://192.168.0.6:22000",
                    "dynamic"
                ],
                "compression": "metadata",
                "certName": "",
                "introducer": False,
                "skipIntroductionRemovals": False,
                "introducedBy": "",
                "paused": False,
                "allowedNetworks": [],
                "autoAcceptFolders": False,
                "maxSendKbps": 0,
                "maxRecvKbps": 0,
                "ignoredFolders": [],
                "maxRequestKiB": 0,
                "untrusted": False,
                "remoteGUIPort": 0,
                "numConnections": 0
            },
            {
                "deviceID": "device-current",
                "name": "Current device",
                "addresses": [
                    "dynamic"
                ],
                "compression": "metadata",
                "certName": "",
                "introducer": False,
                "skipIntroductionRemovals": False,
                "introducedBy": "",
                "paused": False,
                "allowedNetworks": [],
                "autoAcceptFolders": False,
                "maxSendKbps": 0,
                "maxRecvKbps": 0,
                "ignoredFolders": [],
                "maxRequestKiB": 0,
                "untrusted": False,
                "remoteGUIPort": 0,
                "numConnections": 0
            },
            {
                "deviceID": "device-id-2",
                "name": "Phone",
                "addresses": [
                    "dynamic"
                ],
                "compression": "metadata",
                "certName": "",
                "introducer": False,
                "skipIntroductionRemovals": False,
                "introducedBy": "",
                "paused": False,
                "allowedNetworks": [],
                "autoAcceptFolders": False,
                "maxSendKbps": 0,
                "maxRecvKbps": 0,
                "ignoredFolders": [],
                "maxRequestKiB": 0,
                "untrusted": False,
                "remoteGUIPort": 0,
                "numConnections": 0
            },
            {
                "deviceID": "device-id-3",
                "name": "Laptop",
                "addresses": [
                    "tcp://192.168.0.5:22000",
                    "dynamic"
                ],
                "compression": "metadata",
                "certName": "",
                "introducer": False,
                "skipIntroductionRemovals": False,
                "introducedBy": "",
                "paused": False,
                "allowedNetworks": [],
                "autoAcceptFolders": False,
                "maxSendKbps": 0,
                "maxRecvKbps": 0,
                "ignoredFolders": [],
                "maxRequestKiB": 0,
                "untrusted": False,
                "remoteGUIPort": 0,
                "numConnections": 0
            }
        ],
        "gui": {
            "enabled": True,
            "address": "127.0.0.1:8384",
            "unixSocketPermissions": "",
            "user": "",
            "password": "",
            "authMode": "static",
            "useTLS": True,
            "apiKey": "meow :)",
            "insecureAdminAccess": False,
            "theme": "default",
            "debugging": False,
            "insecureSkipHostcheck": False,
            "insecureAllowFrameLoading": False,
            "sendBasicAuthPrompt": False
        },
        "ldap": {
            "address": "",
            "bindDN": "",
            "transport": "plain",
            "insecureSkipVerify": False,
            "searchBaseDN": "",
            "searchFilter": ""
        },
        "options": {
            "listenAddresses": [
                "default"
            ],
            "globalAnnounceServers": [
                "default"
            ],
            "globalAnnounceEnabled": True,
            "localAnnounceEnabled": True,
            "localAnnouncePort": 21027,
            "localAnnounceMCAddr": "[hmm]:21027",
            "maxSendKbps": 0,
            "maxRecvKbps": 0,
            "reconnectionIntervalS": 60,
            "relaysEnabled": True,
            "relayReconnectIntervalM": 10,
            "startBrowser": True,
            "natEnabled": True,
            "natLeaseMinutes": 60,
            "natRenewalMinutes": 30,
            "natTimeoutSeconds": 10,
            "urAccepted": -1,
            "urSeen": 3,
            "urUniqueId": "",
            "urURL": "https://data.syncthing.net/newdata",
            "urPostInsecurely": False,
            "urInitialDelayS": 1800,
            "autoUpgradeIntervalH": 12,
            "upgradeToPreReleases": False,
            "keepTemporariesH": 24,
            "cacheIgnoredFiles": False,
            "progressUpdateIntervalS": 5,
            "limitBandwidthInLan": False,
            "minHomeDiskFree": {
                "value": 1,
                "unit": "%"
            },
            "releasesURL": "https://upgrades.syncthing.net/meta.json",
            "alwaysLocalNets": [],
            "overwriteRemoteDeviceNamesOnConnect": False,
            "tempIndexMinBlocks": 10,
            "unackedNotificationIDs": [],
            "trafficClass": 0,
            "setLowPriority": True,
            "maxFolderConcurrency": 0,
            "crURL": "https://crash.syncthing.net/newcrash",
            "crashReportingEnabled": True,
            "stunKeepaliveStartS": 180,
            "stunKeepaliveMinS": 20,
            "stunServers": [
                "default"
            ],
            "databaseTuning": "auto",
            "maxConcurrentIncomingRequestKiB": 0,
            "announceLANAddresses": True,
            "sendFullIndexOnUpgrade": False,
            "featureFlags": [],
            "connectionLimitEnough": 0,
            "connectionLimitMax": 0,
            "insecureAllowOldTLSVersions": False,
            "connectionPriorityTcpLan": 10,
            "connectionPriorityQuicLan": 20,
            "connectionPriorityTcpWan": 30,
            "connectionPriorityQuicWan": 40,
            "connectionPriorityRelay": 50,
            "connectionPriorityUpgradeThreshold": 0
        },
        "remoteIgnoredDevices": [],
        "defaults": {
            "folder": {
                "id": "",
                "label": "",
                "filesystemType": "basic",
                "path": "~",
                "type": "sendreceive",
                "devices": [
                    {
                        "deviceID": "device-current",
                        "introducedBy": "",
                        "encryptionPassword": ""
                    }
                ],
                "rescanIntervalS": 3600,
                "fsWatcherEnabled": True,
                "fsWatcherDelayS": 10,
                "ignorePerms": False,
                "autoNormalize": True,
                "minDiskFree": {
                    "value": 1,
                    "unit": "%"
                },
                "versioning": {
                    "type": "",
                    "params": {},
                    "cleanupIntervalS": 3600,
                    "fsPath": "",
                    "fsType": "basic"
                },
                "copiers": 0,
                "pullerMaxPendingKiB": 0,
                "hashers": 0,
                "order": "random",
                "ignoreDelete": False,
                "scanProgressIntervalS": 0,
                "pullerPauseS": 0,
                "maxConflicts": 10,
                "disableSparseFiles": False,
                "disableTempIndexes": False,
                "paused": False,
                "weakHashThresholdPct": 25,
                "markerName": ".stfolder",
                "copyOwnershipFromParent": False,
                "modTimeWindowS": 0,
                "maxConcurrentWrites": 2,
                "disableFsync": False,
                "blockPullOrder": "standard",
                "copyRangeMethod": "standard",
                "caseSensitiveFS": False,
                "junctionsAsDirs": False,
                "syncOwnership": False,
                "sendOwnership": False,
                "syncXattrs": False,
                "sendXattrs": False,
                "xattrFilter": {
                    "entries": [],
                    "maxSingleEntrySize": 1024,
                    "maxTotalSize": 4096
                }
            },
            "device": {
                "deviceID": "",
                "name": "",
                "addresses": [
                    "dynamic"
                ],
                "compression": "metadata",
                "certName": "",
                "introducer": False,
                "skipIntroductionRemovals": False,
                "introducedBy": "",
                "paused": False,
                "allowedNetworks": [],
                "autoAcceptFolders": False,
                "maxSendKbps": 0,
                "maxRecvKbps": 0,
                "ignoredFolders": [],
                "maxRequestKiB": 0,
                "untrusted": False,
                "remoteGUIPort": 0,
                "numConnections": 0
            },
            "ignores": {
                "lines": []
            }
        }
    }

@APP.route("/rest/system/version")
def system_version():
    return {
        "arch": "amd64",
        "codename": "Dummy Demo",
        "container": False,
        "date": "2024-01-01T08:44:04+00:00",
        "extra": "",
        "isBeta": False,
        "isCandidate": False,
        "isRelease": True,
        "longVersion": "syncthing version \"Dummy Demo\" (go9.9.9 linux)",
        "os": "linux",
        "stamp": "1702277044",
        "tags": [
            "noupgrade"
        ],
        "user": "debian",
        "version": "v6.6.6"
    }

@APP.route("/rest/system/status")
def system_status():
    return {
        "alloc": 32883448,
        "connectionServiceStatus": {
            "dynamic+https://relays.syncthing.net/endpoint": {
                "error": None,
                "lanAddresses": [
                    "relay://10.1.10.10:22067/?globalLimitBps=0\u0026id=relay-id\u0026networkTimeout=2m0s\u0026pingInterval=1m0s\u0026providedBy=\u0026sessionLimitBps=0\u0026statusAddr=%3A22070"
                ],
                "wanAddresses": [
                    "relay://10.1.10.10:22067/?globalLimitBps=0\u0026id=relay-id\u0026networkTimeout=2m0s\u0026pingInterval=1m0s\u0026providedBy=\u0026sessionLimitBps=0\u0026statusAddr=%3A22070"
                ]
            },
            "quic://0.0.0.0:22000": {
                "error": None,
                "lanAddresses": [
                    "quic://0.0.0.0:22000",
                    "quic://10.1.10.10:22000"
                ],
                "wanAddresses": [
                    "quic://0.0.0.0:22000",
                    "quic://10.1.10.1:22000"
                ]
            },
            "tcp://0.0.0.0:22000": {
                "error": None,
                "lanAddresses": [
                    "tcp://0.0.0.0:22000",
                    "tcp://10.1.10.30:22000"
                ],
                "wanAddresses": [
                    "tcp://0.0.0.0:0",
                    "tcp://0.0.0.0:22000"
                ]
            }
        },
        "cpuPercent": 0,
        "discoveryEnabled": True,
        "discoveryErrors": {
            "global@https://discovery-v6.syncthing.net/v2/": "Post \"https://discovery-v6.syncthing.net/v2/\": dial tcp [2604:a880:400:d0::185c:f001]:443: connect: network is unreachable"
        },
        "discoveryMethods": 5,
        "discoveryStatus": {
            "IPv4 local": {
                "error": None
            },
            "IPv6 local": {
                "error": None
            },
            "global@https://discovery-v4.syncthing.net/v2/": {
                "error": None
            },
            "global@https://discovery-v6.syncthing.net/v2/": {
                "error": "Post \"https://discovery-v6.syncthing.net/v2/\": dial tcp [2604:a880:400:d0::185c:f001]:443: connect: network is unreachable"
            },
            "global@https://discovery.syncthing.net/v2/": {
                "error": None
            }
        },
        "goroutines": 124,
        "guiAddressOverridden": False,
        "guiAddressUsed": "127.0.0.1:8384",
        "lastDialStatus": {
            "tcp://10.1.10.1:22000": {
                "when": "2024-01-01T00:54:44Z",
                "error": "dial tcp 10.1.10.1:22000: connect: no route to host"
            },
            "tcp://192.168.0.4:22000": {
                "when": "2024-01-01T00:54:48Z",
                "error": "dial tcp 192.168.0.4:22000: i/o timeout"
            },
            "tcp://192.168.0.6:22000": {
                "when": "2024-01-01T23:47:38Z",
                "error": "dial tcp 192.168.0.6:22000: i/o timeout"
            },
            "tcp://192.168.0.8:22000": {
                "when": "2024-01-01T23:47:38Z",
                "error": "dial tcp 192.168.0.8:22000: i/o timeout"
            },
            "tcp://10.1.10.3:22000": {
                "when": "2024-01-01T23:47:53Z",
                "error": None
            }
        },
        "myID": "device-current",
        "pathSeparator": "/",
        "startTime": "2024-01-01T01:46:33+00:00",
        "sys": 71459096,
        "tilde": "/home/demo",
        "uptime": 41530000,
        "urVersionMax": 3
    }

@APP.route("/rest/system/connections")
def system_connections():
    return {
        "connections": {
            "device-id-2": {
                "at": "0001-01-01T00:00:00Z",
                "inBytesTotal": randint(1024 ** 1, 1024 ** 4),
                "outBytesTotal": randint(1024 ** 1, 1024 ** 4),
                "startedAt": "0001-01-01T00:00:00Z",
                "connected": False,
                "paused": False,
                "clientVersion": "",
                "address": "",
                "type": "",
                "isLocal": False,
                "crypto": "",
                "primary": {
                    "at": "0001-01-01T00:00:00Z",
                    "inBytesTotal": 0,
                    "outBytesTotal": 0,
                    "startedAt": "0001-01-01T00:00:00Z",
                    "address": "",
                    "type": "",
                    "isLocal": False,
                    "crypto": ""
                }
            },
            "device-id-3": {
                "at": "0001-01-01T00:00:00Z",
                "inBytesTotal": randint(1024 ** 1, 1024 ** 4),
                "outBytesTotal": randint(1024 ** 1, 1024 ** 4),
                "startedAt": "0001-01-01T00:00:00Z",
                "connected": False,
                "paused": False,
                "clientVersion": "",
                "address": "",
                "type": "",
                "isLocal": False,
                "crypto": "",
                "primary": {
                    "at": "0001-01-01T00:00:00Z",
                    "inBytesTotal": 0,
                    "outBytesTotal": 0,
                    "startedAt": "0001-01-01T00:00:00Z",
                    "address": "",
                    "type": "",
                    "isLocal": False,
                    "crypto": ""
                }
            },
            "device-id-1": {
                "at": "2024-01-01T02:56:13+00:00",
                "inBytesTotal": randint(1024 ** 1, 1024 ** 4),
                "outBytesTotal": randint(1024 ** 1, 1024 ** 4),
                "startedAt": "2024-01-01T01:47:54+00:00",
                "connected": True,
                "paused": False,
                "clientVersion": "v6.6.6",
                "address": "10.1.10.2:22000",
                "type": "tcp-client",
                "isLocal": False,
                "crypto": "TLS1.3-TLS",
                "primary": {
                    "at": "2024-01-01T02:56:13+00:00",
                    "inBytesTotal": randint(1024 ** 1, 1024 ** 4),
                    "outBytesTotal": randint(1024 ** 1, 1024 ** 4),
                    "startedAt": "2024-01-01T01:47:54+00:00",
                    "address": "10.1.10.2:22000",
                    "type": "tcp-client",
                    "isLocal": False,
                    "crypto": "TLS1.3-TLS"
                }
            }
        },
        "total": {
            "at": "2024-01-01T02:56:13+00:00",
            "inBytesTotal": randint(1024 ** 1, 1024 ** 4),
            "outBytesTotal": randint(1024 ** 1, 1024 ** 4)
        }
    }

@APP.route("/rest/system/log")
def system_log():
    return {
        "messages": [
            {
                "when": "2024-01-01T01:46:33.511845853+00:00",
                "message": "My ID: device-current",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.419707236+00:00",
                "message": "Single thread SHA256 performance is 1 MB/s using minio/sha256-simd (3 MB/s using crypto/sha256).",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.905295965+00:00",
                "message": "Hashing performance is 2 MB/s",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.907774898+00:00",
                "message": "Overall send rate is unlimited, receive rate is unlimited",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.911946991+00:00",
                "message": "Using discovery mechanism: global discovery server https://discovery.syncthing.net/v2/?noannounce\u0026id=discovery-id",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.912120267+00:00",
                "message": "Using discovery mechanism: global discovery server https://discovery-v4.syncthing.net/v2/?nolookup\u0026id=discovery-id",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.912204264+00:00",
                "message": "Using discovery mechanism: global discovery server https://discovery-v6.syncthing.net/v2/?nolookup\u0026id=discovery-id",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.912364548+00:00",
                "message": "Using discovery mechanism: IPv4 local broadcast discovery on port 21027",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.913127155+00:00",
                "message": "Ready to synchronize \"Notes\" (folder-id-1) (sendreceive)",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.913127339+00:00",
                "message": "...",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.913789629+00:00",
                "message": "TCP listener ([::]:22000) starting",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.913860569+00:00",
                "message": "Relay listener (dynamic+https://relays.syncthing.net/endpoint) starting",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.91639839+00:00",
                "message": "Ready to synchronize \"Photos\" (folder-id-2) (sendreceive)",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.917164578+00:00",
                "message": "Ready to synchronize \"Code\" (folder-id-3) (sendreceive)",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.922825148+00:00",
                "message": "GUI and API listening on 127.0.0.1:8384",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.922834022+00:00",
                "message": "Access the GUI via the following URL: https://127.0.0.1:8384/",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.923886823+00:00",
                "message": "Detected 0 NAT services",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.923900555+00:00",
                "message": "My name is \"Demo\"",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.923941579+00:00",
                "message": "Device device-id-1 is \"Desktop\" at [tcp://10.1.10.1:22000 tcp://192.168.0.1:22000 dynamic]",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.924049581+00:00",
                "message": "Device device-id-2 is \"Phone\" at [dynamic]",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.924133245+00:00",
                "message": "Device device-id-3 is \"y\" at [tcp://10.1.10.2:22000 dynamic]",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:34.932815771+00:00",
                "message": "QUIC listener ([::]:22000) starting",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:35.033604284+00:00",
                "message": "Relay listener (dynamic+https://relays.syncthing.net/endpoint) shutting down",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:35.033768831+00:00",
                "message": "listenerSupervisor@dynamic+https://relays.syncthing.net/endpoint: service dynamic+https://relays.syncthing.net/endpoint failed: Get \"https://relays.syncthing.net/endpoint\": dial tcp: lookup relays.syncthing.net on 127.0.0.53:53: server misbehaving",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:35.033825514+00:00",
                "message": "Relay listener (dynamic+https://relays.syncthing.net/endpoint) starting",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:35.057591863+00:00",
                "message": "Completed initial scan of sendreceive folder \"Photos\" (folder-id-2)",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:35.062180067+00:00",
                "message": "Relay listener (dynamic+https://relays.syncthing.net/endpoint) shutting down",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:35.062278762+00:00",
                "message": "listenerSupervisor@dynamic+https://relays.syncthing.net/endpoint: service dynamic+https://relays.syncthing.net/endpoint failed: Get \"https://relays.syncthing.net/endpoint\": dial tcp: lookup relays.syncthing.net on 127.0.0.53:53: server misbehaving",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:35.062329622+00:00",
                "message": "Relay listener (dynamic+https://relays.syncthing.net/endpoint) starting",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:35.135214538+00:00",
                "message": "Relay listener (dynamic+https://relays.syncthing.net/endpoint) shutting down",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:35.135275627+00:00",
                "message": "listenerSupervisor@dynamic+https://relays.syncthing.net/endpoint: service dynamic+https://relays.syncthing.net/endpoint failed: Get \"https://relays.syncthing.net/endpoint\": dial tcp: lookup relays.syncthing.net on 127.0.0.53:53: server misbehaving",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:38.314413625+00:00",
                "message": "Completed initial scan of sendreceive folder \"Notes\" (folder-id-1)",
                "level": 2
            },
            {
                "when": "2024-01-01T01:46:39.340305333+00:00",
                "message": "Completed initial scan of sendreceive folder \"Code\" (folder-id-3)",
                "level": 2
            },
            {
                "when": "2024-01-01T01:47:50.212171543+00:00",
                "message": "Relay listener (dynamic+https://relays.syncthing.net/endpoint) starting",
                "level": 2
            },
            {
                "when": "2024-01-01T01:47:53.257966908+00:00",
                "message": "Established secure connection to device-id-1 at somewhere/tcp-client/TLS1.3-TLS/WAN-SOMETHING",
                "level": 2
            },
            {
                "when": "2024-01-01T01:47:53.259813923+00:00",
                "message": "Device device-id-1 client is \"syncthing version\" named \"Desktop\" at somewhere/tcp-client/TLS1.3-TLS/WAN-SOMETHING",
                "level": 2
            },
            {
                "when": "2024-01-01T01:48:38.706627903+00:00",
                "message": "Joined relay relay://127.0.0.1:22067",
                "level": 2
            },
            {
                "when": "2024-01-01T01:51:45.386873318+00:00",
                "message": "quic://0.0.0.0:22000 detected NAT type: Yes",
                "level": 2
            },
            {
                "when": "2024-01-01T02:22:53.314860507+00:00",
                "message": "quic://0.0.0.0:22000 detected NAT type: Yes",
                "level": 2
            },
            {
                "when": "2024-01-01T02:22:53.31558948+00:00",
                "message": "quic://0.0.0.0:22000 resolved external address quic://meow (via stun.syncthing.net:3478)",
                "level": 2
            },
            {
                "when": "2024-01-01T02:25:53.358245682+00:00",
                "message": "quic://0.0.0.0:22000 resolved external address quic://bark:22000 (via stun.syncthing.net:3478)",
                "level": 2
            }
        ]
    }

@APP.route("/rest/events/disk")
def disk_changes():
    return [
        {
            "id": 1,
            "globalID": 36,
            "time": "2024-01-01T01:50:06.636965013+00:00",
            "type": "LocalChangeDetected",
            "data": {
                "action": "modified",
                "folder": "folder-id-1",
                "folderID": "folder-id-1",
                "label": "Notes",
                "modifiedBy": "device-id-2",
                "path": ".#note.txt",
                "type": "symlink"
            }
        },
        {
            "id": 2,
            "globalID": 41,
            "time": "2024-01-01T01:52:49.43128229+00:00",
            "type": "LocalChangeDetected",
            "data": {
                "action": "modified",
                "folder": "folder-id-1",
                "folderID": "folder-id-1",
                "label": "Notes",
                "modifiedBy": "device-id-2",
                "path": "#note.txt#",
                "type": "file"
            }
        },
        {
            "id": 4,
            "globalID": 53,
            "time": "2024-01-01T01:59:07.825271167+00:00",
            "type": "LocalChangeDetected",
            "data": {
                "action": "modified",
                "folder": "folder-id-1",
                "folderID": "folder-id-1",
                "label": "Notes",
                "modifiedBy": "device-id-2",
                "path": "note.txt~",
                "type": "file"
            }
        }
    ]

@APP.route("/rest/db/completion")
def db_completion():
    compl = {
        "folder-id-1": 100,
        "folder-id-2": 90,
        "folder-id-3": 78,
        "device-id-1": 55,
        "device-id-2": 30,
        "device-id-3": 0,
    }
    if A("folder") and not A("device"):
        return {
            "completion": compl.get(A("folder"), 0),
            "globalBytes": 892400428200,
            "globalItems": 25777,
            "needBytes": 0,
            "needDeletes": 0,
            "needItems": 0,
            "remoteState": "unknown",
            "sequence": 167814
        }
    elif not A("folder") and A("device"):
        return {
            "completion": compl.get(A("device"), 0),
            "globalBytes": 23889889728,
            "globalItems": 53571,
            "needBytes": 0,
            "needDeletes": 0,
            "needItems": 0,
            "remoteState": "unknown",
            "sequence": 0
        }
    elif A("folder") and A("device"):
        return {
            "completion": compl.get(A("folder"), 0),
            "globalBytes": 892400428200,
            "globalItems": 25777,
            "needBytes": 0,
            "needDeletes": 0,
            "needItems": 0,
            "remoteState": "unknown",
            "sequence": 167814
        }

@APP.route("/rest/db/status")
def db_status():
    return {
        "errors": 0,
        "pullErrors": 0,
        "invalid": "",
        "globalFiles": 3,
        "globalDirectories": 1,
        "globalSymlinks": 0,
        "globalDeleted": 81221,
        "globalBytes": 892400428200,
        "globalTotalItems": 106998,
        "localFiles": 3,
        "localDirectories": 1,
        "localSymlinks": 0,
        "localDeleted": 52136,
        "localBytes": 892400428200,
        "localTotalItems": 77913,
        "needFiles": 0,
        "needDirectories": 0,
        "needSymlinks": 0,
        "needDeletes": 0,
        "needBytes": 0,
        "needTotalItems": 0,
        "receiveOnlyChangedFiles": 0,
        "receiveOnlyChangedDirectories": 0,
        "receiveOnlyChangedSymlinks": 0,
        "receiveOnlyChangedDeletes": 0,
        "receiveOnlyChangedBytes": 0,
        "receiveOnlyTotalItems": 0,
        "inSyncFiles": 3,
        "inSyncBytes": 892400428200,
        "state": "idle",
        "stateChanged": "2024-01-01T02:39:00+00:00",
        "error": "",
        "version": 167814,
        "sequence": 167814,
        "ignorePatterns": False,
        "watchError": ""
    }

@APP.route("/rest/stats/folder")
def stats_folder():
    return {
        "folder-id-1": {
            "lastFile": {
                "at": "2024-01-01T14:31:36+01:00",
                "filename": "note.txt",
                "deleted": False
            },
            "lastScan": "2024-01-01T02:43:27+00:00"
        },
        "folder-id-2": {
            "lastFile": {
                "at": "2024-01-01T01:23:05+01:00",
                "filename": "photo.jpg",
                "deleted": False
            },
            "lastScan": "2024-01-01T02:51:17+00:00"
        },
        "folder-id-3": {
            "lastFile": {
                "at": "2024-01-01T12:47:21+00:00",
                "filename": "code.el",
                "deleted": False
            },
            "lastScan": "2024-01-01T02:45:05+00:00"
        }
    }

@APP.route("/rest/stats/device")
def stats_device():
    return {
        "device-id-2": {
            "lastSeen": "2024-01-01T23:41:28+00:00",
            "lastConnectionDurationS": 3017.948663252
        },
        "device-id-3": {
            "lastSeen": "2024-01-01T01:08:53+01:00",
            "lastConnectionDurationS": 60.962836684
        },
        "device-id-1": {
            "lastSeen": "2024-01-01T02:57:47+00:00",
            "lastConnectionDurationS": 1544.237146131
        },
        "device-current": {
            "lastSeen": "1970-01-01T02:00:00+00:00",
            "lastConnectionDurationS": 0
        }
    }

@APP.route("/rest/events")
def events():
    return []


if __name__ == "__main__":
    APP.run()
