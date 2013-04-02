!
!     module m_stability_data
!.......................................................................
!
!     Written by Kemorin
!
      module m_stability_data
!
      use m_precision
!
      implicit  none
!
      real(kind=kreal) :: min_length, cfl_tmp
! 
      real(kind=kreal) :: cfl_diffuse, cfl_advect
! 
      real(kind=kreal) :: cfl_length0, cfl_advect0
! 
      end module m_stability_data
