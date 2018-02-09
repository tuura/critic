module Types where

data Graph = Graph
            {
                xmlns     :: String,
                graphType :: GraphType,
                graphInstance :: GraphInstance
            }

instance Show Graph where
    show g = "Graph\n" ++
             "    xmlns = " ++ xmlns g ++ "\n" ++
             "    " ++ (show $ graphType g) ++ "\n" ++
             "    " ++ (show $ graphInstance g)

data GraphType = GraphType
               {
                   graphTypeID  :: String,
                   messageTypes :: [MessageType],
                   deviceTypes  :: [DeviceType]
               }

instance Show GraphType where
    show g = "GraphType\n" ++
             "    id = " ++ graphTypeID g ++ "\n" ++
             "        " ++ (show $ messageTypes g) ++ "\n" ++
             "        " ++ (show $ deviceTypes g)

data MessageType = MessageType
                  {
                      messageId :: String,
                      message   :: Message
                  }

instance Show MessageType where
    show m = "MessageType\n" ++
             "        id = " ++ messageId m ++ "\n" ++
             "            " ++ (show $ message m) ++ "\n"

data Message = Message
             {
                 name        :: String,
                 messageType :: String
             }

instance Show Message where
    show m = "Message\n" ++
             "            name = " ++ name m ++ "\n" ++
             "            type = " ++ messageType m

data DeviceType = DeviceType
                {
                    deviceId     :: String,
                    states       :: [State],
                    inputPins    :: [InputPin],
                    outputPins   :: [OutputPin],
                    readyToSend  :: String
                }

instance Show DeviceType where
    show d = "DeviceType\n" ++
             "            " ++ (show $ states d) ++ "\n" ++
             "            " ++ (show $ inputPins d) ++ "\n" ++
             "            " ++ (show $ outputPins d) ++ "\n" ++
             "            ReadyToSend = " ++ readyToSend d ++ "\n"

data State = State
           {
               stateName :: String,
               stateType :: String
           }

instance Show State where
    show s = "State\n" ++
             "            name = " ++ stateName s ++ "\n" ++
             "            type = " ++ stateType s ++ "\n"

data InputPin = InputPin
              {
                  inputName     :: String,
                  iMessageTypeID :: String,
                  onReceive     :: String
              }

instance Show InputPin where
    show i = "InputPin\n" ++
             "            name = " ++ inputName i ++ "\n" ++
             "            messageTypeID = " ++ iMessageTypeID i ++ "\n" ++
             "            onReceive = " ++ onReceive i ++ "\n"

data OutputPin = OutputPin
               {
                   outputName    :: String,
                   oMessageTypeID :: String,
                   onSend        :: String
               }

instance Show OutputPin where
    show o = "OutputPin\n" ++
             "            name = " ++ outputName o ++ "\n" ++
             "            messageTypeID = " ++ oMessageTypeID o ++ "\n" ++
             "            onSend = " ++ onSend o ++ "\n"

data GraphInstance = GraphInstance
                   {
                       instanceGraphTypeID :: String,
                       instanceID          :: String,
                       deviceInstances     :: [DeviceInstance],
                       edgeInstances       :: [EdgeInstance]
                   }

instance Show GraphInstance where
    show g = "GraphInstance\n" ++
             "    graphTypeID = " ++ instanceGraphTypeID g ++ "\n" ++
             "    instanceID = " ++ instanceID g ++ "\n" ++
             "    " ++ (show $ deviceInstances g) ++ "\n" ++
             "    " ++ (show $ edgeInstances g) ++ "\n"

data DeviceInstance = DeviceInstance
                    {
                        deviceType :: String,
                        deviceID   :: String
                    }

instance Show DeviceInstance where
    show d = "DeviceInstance\n" ++
             "        type = " ++ deviceType d ++ "\n" ++
             "        id = " ++ deviceID d ++ "\n"

data EdgeInstance = EdgeInstance
                  {
                      path :: String
                  }

instance Show EdgeInstance where
    show e = "EdgeInstance\n" ++
             "        path = " ++ path e ++ "\n"
