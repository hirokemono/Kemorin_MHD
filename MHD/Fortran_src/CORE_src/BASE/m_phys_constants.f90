!
!     module m_phys_constants
!
!   Written by H. Matsui on May, 2009
!
!>  Constants for physical data
!
      module m_phys_constants
!
      use m_precision
!
      implicit  none
!
!
      integer (kind = kint), parameter :: n_scalar = 1
      integer (kind = kint), parameter :: n_vector = 3
      integer (kind = kint), parameter :: n_sym_tensor =  6
      integer (kind = kint), parameter :: n_asym_tensor = 3
      integer (kind = kint), parameter :: n_solenoid = 2
!
      integer (kind = kint), parameter :: l_sim_t(3,3)                  &
     &                       = reshape( (/0, 1, 2,                      &
     &                                    1, 3, 4,                      &
     &                                    2, 4, 5/), shape=(/3,3/))
!
      integer (kind = kint), parameter :: l_asim_t(3,3,2)               &
     &                = reshape( (/0, 0, 2,   0, 0, 1,  2, 1, 0,        &
     &                             0, 1,-1,  -1, 0, 1,  1,-1, 0 /),     &
     &                  shape=(/3,3,2/) )
!
      integer (kind = kint), parameter :: lst_sim_t(3) = (/0, 3, 5/)
      integer (kind = kint), parameter :: lst_asim_t(3) = (/0, 1, 2/)
!
      end module m_phys_constants
