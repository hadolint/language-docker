module Language.Docker.ParseExposeSpec where

import Data.Default.Class (def)
import qualified Data.Text as Text
import Language.Docker.Syntax
import TestHelper
import Test.Hspec


spec :: Spec
spec = do
  describe "parse EXPOSE" $ do

    it "should handle number ports" $
      let content = "EXPOSE 8080"
       in assertAst
            content
            [ Expose
                ( Ports
                    [ PortSpec (Port 8080 TCP)
                    ]
                )
            ]

    it "should handle many number ports" $
      let content = "EXPOSE 8080 8081"
       in assertAst
            content
            [ Expose
                ( Ports
                    [ PortSpec (Port 8080 TCP),
                      PortSpec (Port 8081 TCP)
                    ]
                )
            ]

    it "should handle ports with protocol" $
      let content = "EXPOSE 8080/TCP 8081/UDP"
       in assertAst
            content
            [ Expose
                ( Ports
                    [ PortSpec (Port 8080 TCP),
                      PortSpec (Port 8081 UDP)
                    ]
                )
            ]

    it "should handle ports with protocol and variables" $
      let content = "EXPOSE $PORT 8080 8081/UDP"
       in assertAst
            content
            [ Expose
                ( Ports
                  [ PortSpec (PortStr "$PORT"),
                    PortSpec (Port 8080 TCP),
                    PortSpec (Port 8081 UDP)
                  ]
                )
            ]

    it "should handle port ranges" $
      let content = "EXPOSE 80 81 8080-8085"
       in assertAst
            content
            [ Expose
                ( Ports
                    [ PortSpec (Port 80 TCP),
                      PortSpec (Port 81 TCP),
                      PortRangeSpec
                        ( PortRange (Port 8080 TCP) (Port 8085 TCP) )
                    ]
                )
            ]

    it "should handle udp port ranges" $
      let content = "EXPOSE 80 81 8080-8085/udp"
       in assertAst
            content
            [ Expose
                ( Ports
                    [ PortSpec (Port 80 TCP),
                      PortSpec (Port 81 TCP),
                      PortRangeSpec
                        ( PortRange (Port 8080 UDP) (Port 8085 UDP) )
                    ]
                )
            ]

    it "should handle one variable port ranges 1" $
      let content = "EXPOSE 8080-${bar}/udp"
       in assertAst
            content
            [ Expose
                ( Ports
                    [ PortRangeSpec
                        ( PortRange (Port 8080 UDP) (PortStr "${bar}") )
                    ]
                )
            ]

    it "should handle one variable port ranges 2" $
      let content = "EXPOSE ${foo}-8080/udp"
       in assertAst
            content
            [ Expose
                ( Ports
                    [ PortRangeSpec
                        ( PortRange (PortStr "${foo}") (Port 8080 UDP) )
                    ]
                )
            ]

    it "should handle two variables port ranges" $
      let content = "EXPOSE ${foo}-${bar}/udp"
       in assertAst
            content
            [ Expose
                ( Ports
                    [ PortRangeSpec
                        ( PortRange (PortStr "${foo}") (PortStr "${bar}") )
                    ]
                )
            ]

    it "should handle multiline variables" $
      let content =
            "EXPOSE  ${PORT} ${PORT_SSL} \\\n\
            \        ${PORT_HTTP} ${PORT_HTTPS} \\\n\
            \        ${PORT_REP} \\\n\
            \        ${PORT_ADMIN} ${PORT_ADMIN_HTTP}"
       in assertAst
            content
            [ Expose
                ( Ports
                    [ PortSpec (PortStr "${PORT}"),
                      PortSpec (PortStr "${PORT_SSL}"),
                      PortSpec (PortStr "${PORT_HTTP}"),
                      PortSpec (PortStr "${PORT_HTTPS}"),
                      PortSpec (PortStr "${PORT_REP}"),
                      PortSpec (PortStr "${PORT_ADMIN}"),
                      PortSpec (PortStr "${PORT_ADMIN_HTTP}")
                    ]
                )
            ]
    it "should fail with wrong protocol" $
      let content = "EXPOSE 80/ip"
       in expectFail content
