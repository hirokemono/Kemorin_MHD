!
!     module   t_iccg_parameter
!.......................................................................
!
!     Written by H. Matsui and H. Okuda in 2000
!
!!      subroutine set_control_4_DJDS_solver(DJDS_ctl, DJDS_param)
!!        type(DJDS_control), intent(in) :: DJDS_ctl
!!        type(DJDS_poarameter), intent(inout) :: DJDS_param
!
      module   t_iccg_parameter
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint), parameter :: iflag_OFF =        0
      integer(kind = kint), parameter :: iflag_MC_RCM =     1
      integer(kind = kint), parameter :: iflag_MultiColor = 2
!
      type CG_poarameter
!>        Maxmum iteration
        integer(kind=kint) :: MAXITR
!>        Error torrance
        real(kind=kreal)   :: EPS
!>        Coefficients for SSOR
        real(kind=kreal)   :: sigma
!>        Coefficients for SSOR
        real(kind=kreal)   :: sigma_diag
! 
!>        Oreconditioning method
        character (len=kchara)   :: PRECOND
!>        olver method
        character (len=kchara)   :: METHOD
      end type CG_poarameter
! 
!
      type DJDS_poarameter
!>      Ordering type for linear solver
        character (len=kchara) :: ordering_name
!
        integer (kind=kint) :: iflag_ordering
        integer (kind=kint) :: min_color
        integer (kind=kint) :: mc_color
      end type DJDS_poarameter
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_DJDS_solver(DJDS_ctl, DJDS_param)
!
      use calypso_mpi
      use m_error_IDs
      use t_ctl_data_4_solvers
      use skip_comment_f
!
      type(DJDS_control), intent(in) :: DJDS_ctl
      type(DJDS_poarameter), intent(inout) :: DJDS_param
!
!
        if (DJDS_ctl%order_method_ctl%iflag .eq. 0) then
              e_message                                                 &
     &         = 'Set ordering scheme for DJDS solver'
              call calypso_MPI_abort(ierr_CG, e_message)
        else
          DJDS_param%ordering_name                                      &
     &         = DJDS_ctl%order_method_ctl%charavalue
        end if
!
        if (cmp_no_case(DJDS_param%ordering_name, 'RCM_DJDS')) then 
          DJDS_param%iflag_ordering = 1
          DJDS_param%mc_color = 0
          if (DJDS_ctl%min_color_ctl%iflag .eq. 0) then
            DJDS_param%min_color = 0
          else
            DJDS_param%min_color = DJDS_ctl%min_color_ctl%intvalue
          end if
        else if(cmp_no_case(DJDS_param%ordering_name,'MC_DJDS')) then
          DJDS_param%iflag_ordering = iflag_MultiColor
          if (DJDS_ctl%mc_color_ctl%iflag .eq. 0) then
            DJDS_param%mc_color = 0
          else
            DJDS_param%mc_color = DJDS_ctl%mc_color_ctl%intvalue
          end if
          DJDS_param%min_color = DJDS_ctl%mc_color_ctl%intvalue
        end if
!
        if (iflag_debug.eq.1) then
          write(*,*) 'ordering_name: , iflag_ordering ',                &
     &      trim(DJDS_param%ordering_name), DJDS_param%iflag_ordering
          write(*,*) 'min_color:         ', DJDS_param%min_color
          write(*,*) 'mc_color:          ', DJDS_param%mc_color
        end if
!
      end subroutine set_control_4_DJDS_solver
!
! -----------------------------------------------------------------------
!
      end module t_iccg_parameter
