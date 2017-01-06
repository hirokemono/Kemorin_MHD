!
!     module   m_iccg_parameter
!.......................................................................
!
!     Written by H. Matsui and H. Okuda in 2000
!
!!      subroutine set_control_4_DJDS_solver(DJDS_ctl)
!!        type(DJDS_control), intent(in) :: DJDS_ctl
!
      module   m_iccg_parameter
!
      use m_precision
!
      implicit  none
!
      real(kind=kreal)   :: eps
      integer(kind=kint) :: itr
      real(kind=kreal)   :: sigma
      real(kind=kreal)   :: sigma_diag
! 
      character (len=kchara)   :: precond
      character (len=kchara)   :: method
! 
!
      real(kind=kreal), dimension(3)   :: init_4_solver11_real
      integer(kind=kint), dimension(2)   :: init_4_solver11_int
! 
!
      real(kind=kreal), dimension(3)   :: init_4_solver33_real
      integer(kind=kint), dimension(2)   :: init_4_solver33_int
! 
!      solver and precionditioning for Poisson matrix
!
      character (len=kchara) :: precond_4_solver
      character (len=kchara) :: method_4_solver
! 
!      solver and precionditioning for 3x3 matrix
!
      character (len=kchara) :: precond_4_crank
      character (len=kchara) :: method_4_velo
! 
      real(kind=kreal) :: eps_crank
! 
      real(kind=kreal)   :: eps_4_velo_crank =  0.0d0
      real(kind=kreal)   :: eps_4_magne_crank = 0.0d0
      real(kind=kreal)   :: eps_4_temp_crank =  0.0d0
      real(kind=kreal)   :: eps_4_comp_crank =  0.0d0
!
!
      integer(kind=kint) :: itr_res
!
!>      Ordering type for linear solver
      character (len=kchara) :: ordering_name
!
      integer (kind=kint) :: iflag_ordering
      integer (kind=kint) :: min_color
      integer (kind=kint) :: mc_color
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_DJDS_solver(DJDS_ctl)
!
      use calypso_mpi
      use m_error_IDs
      use t_ctl_data_4_solvers
      use skip_comment_f
!
      type(DJDS_control), intent(in) :: DJDS_ctl
!
!
        if (DJDS_ctl%order_method_ctl%iflag .eq. 0) then
              e_message                                                 &
     &         = 'Set ordering scheme for DJDS solver'
              call calypso_MPI_abort(ierr_CG, e_message)
        else
          ordering_name = DJDS_ctl%order_method_ctl%charavalue
        end if
!
        if (cmp_no_case(ordering_name, 'RCM_DJDS')) then 
          iflag_ordering = 1
          mc_color = 0
          if (DJDS_ctl%min_color_ctl%iflag .eq. 0) then
            min_color = 0
          else
            min_color = DJDS_ctl%min_color_ctl%intvalue
          end if
        else if  (cmp_no_case(ordering_name,'MC_DJDS')) then
          iflag_ordering = 2
          if (DJDS_ctl%mc_color_ctl%iflag .eq. 0) then
            mc_color = 0
          else
            mc_color = DJDS_ctl%mc_color_ctl%intvalue
          end if
          min_color = DJDS_ctl%mc_color_ctl%intvalue
        end if
!
        if (iflag_debug.eq.1) then
          write(*,*) 'ordering_name: , iflag_ordering ',                &
     &                trim(ordering_name), iflag_ordering
          write(*,*) 'min_color:         ', min_color
          write(*,*) 'mc_color:          ', mc_color
        end if
!
      end subroutine set_control_4_DJDS_solver
!
! -----------------------------------------------------------------------
!
      end module m_iccg_parameter
