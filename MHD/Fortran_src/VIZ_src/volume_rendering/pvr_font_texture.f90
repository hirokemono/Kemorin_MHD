!
!      module pvr_font_texture
!
      module pvr_font_texture
!
!      Written by H. Matsui on July, 2006
!
      use m_constants
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint), parameter ::  ift5_0(25) = (/ 0,1,1,1,0,    &
     &                                                    0,1,0,1,0,    &
     &                                                    0,1,0,1,0,    &
     &                                                    0,1,0,1,0,    &
     &                                                    0,1,1,1,0 /)

      integer(kind = kint), parameter ::  ift5_1(25) = (/ 0,1,1,0,0,    &
     &                                                    0,0,1,0,0,    &
     &                                                    0,0,1,0,0,    &
     &                                                    0,0,1,0,0,    &
     &                                                    0,1,1,1,0 /)

      integer(kind = kint), parameter ::  ift5_2(25) = (/ 0,1,1,1,0,    &
     &                                                    0,0,0,1,0,    &
     &                                                    0,1,1,1,0,    &
     &                                                    0,1,0,0,0,    &
     &                                                    0,1,1,1,0 /)

      integer(kind = kint), parameter ::  ift5_3(25) = (/ 0,1,1,1,0,    &
     &                                                    0,0,0,1,0,    &
     &                                                    0,1,1,1,0,    &
     &                                                    0,0,0,1,0,    &
     &                                                    0,1,1,1,0 /)

      integer(kind = kint), parameter ::  ift5_4(25) = (/ 0,0,1,1,0,    &
     &                                                    0,1,0,1,0,    &
     &                                                    0,1,1,1,0,    &
     &                                                    0,0,0,1,0,    &
     &                                                    0,0,0,1,0 /)

      integer(kind = kint), parameter ::  ift5_5(25) = (/ 0,1,1,1,0,    &
     &                                                    0,1,0,0,0,    &
     &                                                    0,1,1,1,0,    &
     &                                                    0,0,0,1,0,    &
     &                                                    0,1,1,1,0 /)

      integer(kind = kint), parameter ::  ift5_6(25) = (/ 0,1,1,1,0,    &
     &                                                    0,1,0,0,0,    &
     &                                                    0,1,1,1,0,    &
     &                                                    0,1,0,1,0,    &
     &                                                    0,1,1,1,0 /)

      integer(kind = kint), parameter ::  ift5_7(25) = (/ 0,1,1,1,0,    &
     &                                                    0,0,0,1,0,    &
     &                                                    0,0,0,1,0,    &
     &                                                    0,0,0,1,0,    &
     &                                                    0,0,0,1,0 /)

      integer(kind = kint), parameter ::  ift5_8(25) = (/ 0,1,1,1,0,    &
     &                                                    0,1,0,1,0,    &
     &                                                    0,1,1,1,0,    &
     &                                                    0,1,0,1,0,    &
     &                                                    0,1,1,1,0 /)

      integer(kind = kint), parameter ::  ift5_9(25) = (/ 0,1,1,1,0,    &
     &                                                    0,1,0,1,0,    &
     &                                                    0,1,1,1,0,    &
     &                                                    0,0,0,1,0,    &
     &                                                    0,1,1,1,0 /)

      integer(kind = kint), parameter ::  ift5_e(25) = (/ 0,1,1,1,0,    &
     &                                                    0,1,0,0,0,    &
     &                                                    0,1,1,1,0,    &
     &                                                    0,1,0,0,0,    &
     &                                                    0,1,1,1,0 /)

      integer(kind = kint), parameter ::  ift5_d(25) = (/ 0,0,0,0,0,    &
     &                                                    0,0,0,0,0,    &
     &                                                    0,0,0,0,0,    &
     &                                                    0,1,1,0,0,    &
     &                                                    0,1,1,0,0 /)

      integer(kind = kint), parameter ::  ift5_a(25) = (/ 0,0,0,0,0,    &
     &                                                    0,0,1,0,0,    &
     &                                                    0,1,1,1,0,    &
     &                                                    0,0,1,0,0,    &
     &                                                    0,0,0,0,0 /)

      integer(kind = kint), parameter ::  ift5_s(25) = (/ 0,0,0,0,0,    &
     &                                                    0,0,0,0,0,    &
     &                                                    0,1,1,1,0,    &
     &                                                    0,0,0,0,0,    &
     &                                                    0,0,0,0,0 /)


      integer(kind = kint), parameter :: ift7_0(49) = (/0,1,1,1,1,1,0,  &
     &                                                  0,1,0,0,0,1,0,  &
     &                                                  0,1,0,0,0,1,0,  &
     &                                                  0,1,0,0,0,1,0,  &
     &                                                  0,1,0,0,0,1,0,  &
     &                                                  0,1,0,0,0,1,0,  &
     &                                                  0,1,1,1,1,1,0/)
      
      integer(kind = kint), parameter :: ift7_1(49) = (/0,0,0,1,0,0,0,  &
     &                                                  0,0,0,1,0,0,0,  &
     &                                                  0,0,0,1,0,0,0,  &
     &                                                  0,0,0,1,0,0,0,  &
     &                                                  0,0,0,1,0,0,0,  &
     &                                                  0,0,0,1,0,0,0,  &
     &                                                  0,0,0,1,0,0,0/)
      
      integer(kind = kint), parameter :: ift7_2(49) = (/0,1,1,1,1,1,0,  &
     &                                                  0,0,0,0,0,1,0,  &
     &                                                  0,0,0,0,0,1,0,  &
     &                                                  0,1,1,1,1,1,0,  &
     &                                                  0,1,0,0,0,0,0,  &
     &                                                  0,1,0,0,0,0,0,  &
     &                                                  0,1,1,1,1,1,0/)
      
      integer(kind = kint), parameter :: ift7_3(49) = (/0,1,1,1,1,1,0,  &
     &                                                  0,0,0,0,0,1,0,  &
     &                                                  0,0,0,0,0,1,0,  &
     &                                                  0,1,1,1,1,1,0,  &
     &                                                  0,0,0,0,0,1,0,  &
     &                                                  0,0,0,0,0,1,0,  &
     &                                                  0,1,1,1,1,1,0/)
      
      integer(kind = kint), parameter :: ift7_4(49) = (/0,0,1,0,1,0,0,  &
     &                                                  0,1,0,0,1,0,0,  &
     &                                                  0,1,0,0,1,0,0,  &
     &                                                  0,1,1,1,1,1,0,  &
     &                                                  0,0,0,0,1,0,0,  &
     &                                                  0,0,0,0,1,0,0,  &
     &                                                  0,0,0,0,1,0,0/)
      
      integer(kind = kint), parameter :: ift7_5(49) = (/0,1,1,1,1,1,0,  &
     &                                                  0,1,0,0,0,0,0,  &
     &                                                  0,1,0,0,0,0,0,  &
     &                                                  0,1,1,1,1,1,0,  &
     &                                                  0,0,0,0,0,1,0,  &
     &                                                  0,0,0,0,0,1,0,  &
     &                                                  0,1,1,1,1,1,0/)
      
      integer(kind = kint), parameter :: ift7_6(49) = (/0,1,1,1,1,1,0,  &
     &                                                  0,1,0,0,0,0,0,  &
     &                                                  0,1,0,0,0,0,0,  &
     &                                                  0,1,1,1,1,1,0,  &
     &                                                  0,1,0,0,0,1,0,  &
     &                                                  0,1,0,0,0,1,0,  &
     &                                                  0,1,1,1,1,1,0/)
      
      integer(kind = kint), parameter :: ift7_7(49) = (/0,1,1,1,1,1,0,  &
     &                                                  0,0,0,0,0,1,0,  &
     &                                                  0,0,0,0,0,1,0,  &
     &                                                  0,0,0,0,1,0,0,  &
     &                                                  0,0,0,0,1,0,0,  &
     &                                                  0,0,0,0,1,0,0,  &
     &                                                  0,0,0,0,1,0,0/)
      
      integer(kind = kint), parameter :: ift7_8(49) = (/0,1,1,1,1,1,0,  &
     &                                                  0,1,0,0,0,1,0,  &
     &                                                  0,1,0,0,0,1,0,  &
     &                                                  0,1,1,1,1,1,0,  &
     &                                                  0,1,0,0,0,1,0,  &
     &                                                  0,1,0,0,0,1,0,  &
     &                                                  0,1,1,1,1,1,0/)
      
      integer(kind = kint), parameter :: ift7_9(49) = (/0,1,1,1,1,1,0,  &
     &                                                  0,1,0,0,0,1,0,  &
     &                                                  0,1,0,0,0,1,0,  &
     &                                                  0,1,1,1,1,1,0,  &
     &                                                  0,0,0,0,0,1,0,  &
     &                                                  0,0,0,0,0,1,0,  &
     &                                                  0,1,1,1,1,1,0/)
      
      integer(kind = kint), parameter :: ift7_e(49) = (/0,1,1,1,1,1,0,  &
     &                                                  0,1,0,0,0,0,0,  &
     &                                                  0,1,0,0,0,0,0,  &
     &                                                  0,1,1,1,1,1,0,  &
     &                                                  0,1,0,0,0,0,0,  &
     &                                                  0,1,0,0,0,0,0,  &
     &                                                  0,1,1,1,1,1,0/)
      
      integer(kind = kint), parameter :: ift7_d(49) = (/0,0,0,0,0,0,0,  &
     &                                                  0,0,0,0,0,0,0,  &
     &                                                  0,0,0,0,0,0,0,  &
     &                                                  0,0,0,0,0,0,0,  &
     &                                                  0,0,0,0,0,0,0,  &
     &                                                  0,0,1,1,0,0,0,  &
     &                                                  0,0,1,1,0,0,0/)
      
      integer(kind = kint), parameter :: ift7_a(49) = (/0,0,0,0,0,0,0,  &
     &                                                  0,0,0,1,0,0,0,  &
     &                                                  0,0,0,1,0,0,0,  &
     &                                                  0,1,1,1,1,1,0,  &
     &                                                  0,0,0,1,0,0,0,  &
     &                                                  0,0,0,1,0,0,0,  &
     &                                                  0,0,0,0,0,0,0/)
      
      integer(kind = kint), parameter :: ift7_s(49) = (/0,0,0,0,0,0,0,  &
     &                                                  0,0,0,0,0,0,0,  &
     &                                                  0,0,0,0,0,0,0,  &
     &                                                  0,1,1,1,1,1,0,  &
     &                                                  0,0,0,0,0,0,0,  &
     &                                                  0,0,0,0,0,0,0,  &
     &                                                  0,0,0,0,0,0,0/)
!
!
      integer(kind=kint), parameter :: ift8_0(96) = (/0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,1,1,0,0,0,  &
     &                                                0,0,1,0,0,1,0,0,  &
     &                                                0,1,0,0,0,0,1,0,  &
     &                                                0,1,0,0,0,0,1,0,  &
     &                                                0,1,0,0,0,0,1,0,  &
     &                                                0,1,0,0,0,0,1,0,  &
     &                                                0,1,0,0,0,0,1,0,  &
     &                                                0,1,0,0,0,0,1,0,  &
     &                                                0,0,1,0,0,1,0,0,  &
     &                                                0,0,0,1,1,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0/)
      
      integer(kind=kint), parameter :: ift8_1(96) = (/0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,1,0,0,0,  &
     &                                                0,0,0,1,1,0,0,0,  &
     &                                                0,0,1,0,1,0,0,0,  &
     &                                                0,0,0,0,1,0,0,0,  &
     &                                                0,0,0,0,1,0,0,0,  &
     &                                                0,0,0,0,1,0,0,0,  &
     &                                                0,0,0,0,1,0,0,0,  &
     &                                                0,0,0,0,1,0,0,0,  &
     &                                                0,0,0,0,1,0,0,0,  &
     &                                                0,0,0,0,1,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0/)
      
      integer(kind=kint), parameter :: ift8_2(96) = (/0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,1,1,0,0,0,  &
     &                                                0,0,1,0,0,1,0,0,  &
     &                                                0,1,0,0,0,0,1,0,  &
     &                                                0,0,0,0,0,0,1,0,  &
     &                                                0,0,0,0,0,1,1,0,  &
     &                                                0,0,0,0,1,1,0,0,  &
     &                                                0,0,0,1,1,0,0,0,  &
     &                                                0,0,1,1,0,0,0,0,  &
     &                                                0,1,1,0,0,0,0,0,  &
     &                                                0,1,1,1,1,1,1,0,  &
     &                                                0,0,0,0,0,0,0,0/)
      
      integer(kind=kint), parameter :: ift8_3(96) = (/0,0,0,0,0,0,0,0,  &
     &                                                0,0,1,1,1,0,0,0,  &
     &                                                0,1,0,0,0,1,0,0,  &
     &                                                0,0,0,0,0,0,1,0,  &
     &                                                0,0,0,0,0,1,0,0,  &
     &                                                0,0,1,1,1,1,0,0,  &
     &                                                0,0,0,0,0,1,0,0,  &
     &                                                0,0,0,0,0,0,1,0,  &
     &                                                0,0,0,0,0,0,1,0,  &
     &                                                0,1,0,0,0,1,0,0,  &
     &                                                0,0,1,1,1,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0/)
      
      integer(kind=kint), parameter :: ift8_4(96) = (/0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,1,0,0,0,  &
     &                                                0,0,0,1,1,0,0,0,  &
     &                                                0,0,1,0,1,0,0,0,  &
     &                                                0,0,1,0,1,0,0,0,  &
     &                                                0,1,0,0,1,0,0,0,  &
     &                                                0,1,0,0,1,0,0,0,  &
     &                                                0,1,1,1,1,1,1,0,  &
     &                                                0,0,0,0,1,0,0,0,  &
     &                                                0,0,0,0,1,0,0,0,  &
     &                                                0,0,0,0,1,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0/)
      
      integer(kind=kint), parameter :: ift8_5(96) = (/0,0,0,0,0,0,0,0,  &
     &                                                0,1,1,1,1,1,1,0,  &
     &                                                0,1,0,0,0,0,0,0,  &
     &                                                0,1,0,0,0,0,0,0,  &
     &                                                0,1,0,1,1,0,0,0,  &
     &                                                0,1,1,0,0,1,0,0,  &
     &                                                0,1,0,0,0,0,1,0,  &
     &                                                0,0,0,0,0,0,1,0,  &
     &                                                0,0,0,0,0,0,1,0,  &
     &                                                0,1,0,0,0,1,0,0,  &
     &                                                0,0,1,1,1,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0/)
      
      integer(kind=kint), parameter :: ift8_6(96) = (/0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,1,1,0,0,0,  &
     &                                                0,0,1,0,0,1,0,0,  &
     &                                                0,1,1,0,0,0,1,0,  &
     &                                                0,1,0,0,0,0,0,0,  &
     &                                                0,1,0,0,0,0,0,0,  &
     &                                                0,1,1,1,1,1,0,0,  &
     &                                                0,1,0,0,0,1,1,0,  &
     &                                                0,1,0,0,0,0,1,0,  &
     &                                                0,1,1,0,0,1,1,0,  &
     &                                                0,0,1,1,1,1,0,0,  &
     &                                                0,0,0,0,0,0,0,0/)
      
      integer(kind=kint), parameter :: ift8_7(96) = (/0,0,0,0,0,0,0,0,  &
     &                                                0,1,1,1,1,1,1,0,  &
     &                                                0,0,0,0,0,0,1,0,  &
     &                                                0,0,0,0,0,0,1,0,  &
     &                                                0,0,0,0,0,1,1,0,  &
     &                                                0,0,0,0,1,1,0,0,  &
     &                                                0,0,0,1,1,0,0,0,  &
     &                                                0,0,0,1,0,0,0,0,  &
     &                                                0,0,1,1,0,0,0,0,  &
     &                                                0,0,1,0,0,0,0,0,  &
     &                                                0,0,1,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0/)
      
      integer(kind=kint), parameter :: ift8_8(96) = (/0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,1,1,0,0,0,  &
     &                                                0,0,1,0,0,1,0,0,  &
     &                                                0,1,0,0,0,0,1,0,  &
     &                                                0,0,1,0,0,1,0,0,  &
     &                                                0,0,1,1,1,1,0,0,  &
     &                                                0,0,1,0,0,1,0,0,  &
     &                                                0,1,0,0,0,0,1,0,  &
     &                                                0,1,0,0,0,0,1,0,  &
     &                                                0,1,1,0,0,1,1,0,  &
     &                                                0,0,1,1,1,1,0,0,  &
     &                                                0,0,0,0,0,0,0,0/)
      
      integer(kind=kint), parameter :: ift8_9(96) = (/0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,1,1,0,0,0,  &
     &                                                0,0,1,0,0,1,0,0,  &
     &                                                0,1,0,0,0,0,1,0,  &
     &                                                0,1,0,0,0,0,1,0,  &
     &                                                0,1,0,0,0,1,1,0,  &
     &                                                0,0,1,1,1,0,1,0,  &
     &                                                0,0,0,0,0,0,1,0,  &
     &                                                0,0,0,0,0,0,1,0,  &
     &                                                0,1,1,0,0,1,1,0,  &
     &                                                0,0,1,1,1,1,0,0,  &
     &                                                0,0,0,0,0,0,0,0/)
      
      integer(kind=kint), parameter :: ift8_e(96) = (/0,0,0,0,0,0,0,0,  &
     &                                                0,1,1,1,1,1,1,0,  &
     &                                                0,1,0,0,0,0,0,0,  &
     &                                                0,1,0,0,0,0,0,0,  &
     &                                                0,1,0,0,0,0,0,0,  &
     &                                                0,1,0,0,0,0,0,0,  &
     &                                                0,1,1,1,1,1,0,0,  &
     &                                                0,1,0,0,0,0,0,0,  &
     &                                                0,1,0,0,0,0,0,0,  &
     &                                                0,1,0,0,0,0,0,0,  &
     &                                                0,1,1,1,1,1,1,0,  &
     &                                                0,0,0,0,0,0,0,0/)
      
      integer(kind=kint), parameter :: ift8_d(96) = (/0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,1,1,0,0,0,  &
     &                                                0,0,0,1,1,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0/)
      
      integer(kind=kint), parameter :: ift8_a(96) = (/0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,1,0,0,0,  &
     &                                                0,0,0,0,1,0,0,0,  &
     &                                                0,0,0,0,1,0,0,0,  &
     &                                                0,0,1,1,1,1,1,0,  &
     &                                                0,0,0,0,1,0,0,0,  &
     &                                                0,0,0,0,1,0,0,0,  &
     &                                                0,0,0,0,1,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0/)
      
      integer(kind=kint), parameter :: ift8_s(96) = (/0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,1,1,1,1,1,1,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0,  &
     &                                                0,0,0,0,0,0,0,0/)
!
!
      private :: ift5_0, ift5_1, ift5_2, ift5_3, ift5_4, ift5_5, ift5_6
      private :: ift5_7, ift5_8, ift5_9, ift5_e, ift5_d, ift5_a, ift5_s
      private :: ift7_0, ift7_1, ift7_2, ift7_3, ift7_4, ift7_5, ift7_6
      private :: ift7_7, ift7_8, ift7_9, ift7_e, ift7_d, ift7_a, ift7_s
      private :: ift8_0, ift8_1, ift8_2, ift8_3, ift8_4, ift8_5, ift8_6
      private :: ift8_7, ift8_8, ift8_9, ift8_e, ift8_d, ift8_a, ift8_s
!
!      subroutine gen_font5(chara, font_out)
!      subroutine gen_font7(chara, font_out)
!      subroutine gen_font8_12(chara, font_out)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gen_font5(chara, font_out)
!
      character(len = 1), intent(in) :: chara
      integer(kind = kint), intent(inout) :: font_out(5,5)
!
      integer(kind = kint) :: j
!
!
      if      (chara .eq. '0') then
        do j = 1, 5
          font_out(1:5,j) = ift5_0( (5*j-4):(5*j) )
        end do
      else if (chara .eq. '1') then
        do j = 1, 5
          font_out(1:5,j) = ift5_1( (5*j-4):(5*j) )
        end do
      else if (chara .eq. '2') then
        do j = 1, 5
          font_out(1:5,j) = ift5_2( (5*j-4):(5*j) )
        end do
      else if (chara .eq. '3') then
        do j = 1, 5
          font_out(1:5,j) = ift5_3( (5*j-4):(5*j) )
        end do
      else if (chara .eq. '4') then
        do j = 1, 5
          font_out(1:5,j) = ift5_4( (5*j-4):(5*j) )
        end do
      else if (chara .eq. '5') then
        do j = 1, 5
          font_out(1:5,j) = ift5_5( (5*j-4):(5*j) )
        end do
      else if (chara .eq. '6') then
        do j = 1, 5
          font_out(1:5,j) = ift5_6( (5*j-4):(5*j) )
        end do
      else if (chara .eq. '7') then
        do j = 1, 5
          font_out(1:5,j) = ift5_7( (5*j-4):(5*j) )
        end do
      else if (chara .eq. '8') then
        do j = 1, 5
          font_out(1:5,j) = ift5_8( (5*j-4):(5*j) )
        end do
      else if (chara .eq. '9') then
        do j = 1, 5
          font_out(1:5,j) = ift5_9( (5*j-4):(5*j) )
        end do
      else if (chara .eq. 'e' .or. chara .eq. 'E') then
        do j = 1, 5
          font_out(1:5,j) = ift5_e( (5*j-4):(5*j) )
        end do
      else if (chara .eq. '.') then
        do j = 1, 5
          font_out(1:5,j) = ift5_d( (5*j-4):(5*j) )
        end do
      else if (chara .eq. '+') then
        do j = 1, 5
          font_out(1:5,j) = ift5_a( (5*j-4):(5*j) )
        end do
      else if (chara .eq. '-') then
        do j = 1, 5
          font_out(1:5,j) = ift5_s( (5*j-4):(5*j) )
        end do
      else if (chara .eq. ' ') then
        do j = 1, 5
          font_out(1:5,j) = 0
        end do
      end if
!
      end subroutine gen_font5
!
!  ---------------------------------------------------------------------
!
      subroutine gen_font7(chara, font_out)
!
      character(len = 1), intent(in) :: chara
      integer(kind = kint), intent(inout) :: font_out(7,7)
!
      integer(kind = kint) :: j
!
!
      if      (chara .eq. '0') then
        do j = 1, 7
          font_out(1:7,j) = ift7_0( (7*j-6):(7*j) )
        end do
      else if (chara .eq. '1') then
        do j = 1, 7
          font_out(1:7,j) = ift7_1( (7*j-6):(7*j) )
        end do
      else if (chara .eq. '2') then
        do j = 1, 7
          font_out(1:7,j) = ift7_2( (7*j-6):(7*j) )
        end do
      else if (chara .eq. '3') then
        do j = 1, 7
          font_out(1:7,j) = ift7_3( (7*j-6):(7*j) )
        end do
      else if (chara .eq. '4') then
        do j = 1, 7
          font_out(1:7,j) = ift7_4( (7*j-6):(7*j) )
        end do
      else if (chara .eq. '5') then
        do j = 1, 7
          font_out(1:7,j) = ift7_5( (7*j-6):(7*j) )
        end do
      else if (chara .eq. '6') then
        do j = 1, 7
          font_out(1:7,j) = ift7_6( (7*j-6):(7*j) )
        end do
      else if (chara .eq. '7') then
        do j = 1, 7
          font_out(1:7,j) = ift7_7( (7*j-6):(7*j) )
        end do
      else if (chara .eq. '8') then
        do j = 1, 7
          font_out(1:7,j) = ift7_8( (7*j-6):(7*j) )
        end do
      else if (chara .eq. '9') then
        do j = 1, 7
          font_out(1:7,j) = ift7_9( (7*j-6):(7*j) )
        end do
      else if (chara .eq. 'e' .or. chara .eq. 'E') then
        do j = 1, 7
          font_out(1:7,j) = ift7_e( (7*j-6):(7*j) )
        end do
      else if (chara .eq. '.') then
        do j = 1, 7
          font_out(1:7,j) = ift7_d( (7*j-6):(7*j) )
        end do
      else if (chara .eq. '+') then
        do j = 1, 7
          font_out(1:7,j) = ift7_a( (7*j-6):(7*j) )
        end do
      else if (chara .eq. '-') then
        do j = 1, 7
          font_out(1:7,j) = ift7_s( (7*j-6):(7*j) )
        end do
      else if (chara .eq. ' ') then
        do j = 1, 17
          font_out(1:7,j) = 0
        end do
      end if
!
      end subroutine gen_font7
!
!  ---------------------------------------------------------------------
!
      subroutine gen_font8_12(chara, font_out)
!
      character(len = 1), intent(in) :: chara
      integer(kind = kint), intent(inout) :: font_out(8,12)
!
      integer(kind = kint) :: j
!
!
      if      (chara .eq. '0') then
        do j = 1, 12
          font_out(1:8,j) = ift8_0( (8*j-7):(8*j) )
        end do
      else if (chara .eq. '1') then
        do j = 1, 12
          font_out(1:8,j) = ift8_1( (8*j-7):(8*j) )
        end do
      else if (chara .eq. '2') then
        do j = 1, 12
          font_out(1:8,j) = ift8_2( (8*j-7):(8*j) )
        end do
      else if (chara .eq. '3') then
        do j = 1, 12
          font_out(1:8,j) = ift8_3( (8*j-7):(8*j) )
        end do
      else if (chara .eq. '4') then
        do j = 1, 12
          font_out(1:8,j) = ift8_4( (8*j-7):(8*j) )
        end do
      else if (chara .eq. '5') then
        do j = 1, 12
          font_out(1:8,j) = ift8_5( (8*j-7):(8*j) )
        end do
      else if (chara .eq. '6') then
        do j = 1, 12
          font_out(1:8,j) = ift8_6( (8*j-7):(8*j) )
        end do
      else if (chara .eq. '7') then
        do j = 1, 12
          font_out(1:8,j) = ift8_7( (8*j-7):(8*j) )
        end do
      else if (chara .eq. '8') then
        do j = 1, 12
          font_out(1:8,j) = ift8_8( (8*j-7):(8*j) )
        end do
      else if (chara .eq. '9') then
        do j = 1, 12
          font_out(1:8,j) = ift8_9( (8*j-7):(8*j) )
        end do
      else if (chara .eq. 'e' .or. chara .eq. 'E') then
        do j = 1, 12
          font_out(1:8,j) = ift8_e( (8*j-7):(8*j) )
        end do
      else if (chara .eq. '.') then
        do j = 1, 12
          font_out(1:8,j) = ift8_d( (8*j-7):(8*j) )
        end do
      else if (chara .eq. '+') then
        do j = 1, 12
          font_out(1:8,j) = ift8_a( (8*j-7):(8*j) )
        end do
      else if (chara .eq. '-') then
        do j = 1, 12
          font_out(1:8,j) = ift8_s( (8*j-7):(8*j) )
        end do
      else if (chara .eq. ' ') then
        do j = 1, 12
          font_out(1:8,j) = 0
        end do
      end if
!
      end subroutine gen_font8_12
!
!  ---------------------------------------------------------------------
!
      end module pvr_font_texture
