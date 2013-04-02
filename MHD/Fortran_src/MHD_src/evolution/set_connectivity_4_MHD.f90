!
!     module set_connectivity_4_MHD
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on July, 2006
!
!      subroutine set_connectivity
!
      module set_connectivity_4_MHD
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_connectivity
!
      use m_machine_parameter
      use set_MHD_connectivity
      use set_residual_limit
!
!
!      set matrix connectivity
!
      call set_residual_4_crank
!
      if (iflag_debug.eq.1)  write(*,*) 'set_connectivity_whole'
      call set_connectivity_whole
!
      if (iflag_debug.eq.1)   write(*,*) 'set_connectivity_fluid'
      call set_connectivity_fluid
!
!      if (iflag_debug.eq.1)   write(*,*) 'set_connectivity_conduct'
!      call set_connectivity_conduct
!
!      if (iflag_debug.eq.1)   write(*,*) 'set_connectivity_insulate'
!      call set_connectivity_insulate
!
!
      if (iflag_debug.eq.1)  write(*,*) 'set_connectivity_linear'
      call set_connectivity_linear
!
      if (iflag_debug.eq.1)  write(*,*) 'set_connectivity_linear_fl'
      call set_connectivity_linear_fl
!
!      if (iflag_debug.eq.1)  write(*,*) 'set_connectivity_linear_cd'
!      call set_connectivity_linear_cd
!
!      if (iflag_debug.eq.1)  write(*,*) 'set_connectivity_linear_ins'
!      call set_connectivity_linear_ins
!
!
      end subroutine set_connectivity
!
!-----------------------------------------------------------------------
!
      end module set_connectivity_4_MHD
