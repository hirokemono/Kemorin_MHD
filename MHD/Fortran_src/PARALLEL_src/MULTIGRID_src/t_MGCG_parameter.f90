!t_MGCG_parameter.f90
!      module t_MGCG_parameter
!
!     Written by H. Matsui on Dec., 2008
!
!!      subroutine set_MGCG_parameter(MG_ctl, MG_param)
!!        type(MGCG_control), intent(inout) :: MG_ctl!
!!        type(MGCG_parameter), intent(inout) :: MG_param
!
      module t_MGCG_parameter
!
      use m_precision
!
      implicit  none
!
!   parameteres for multigrid
!
      type MGCG_parameter
        character (len=kchara) :: METHOD_MG =  'CG'
        character (len=kchara) :: PRECOND_MG = 'DIAG'
        integer(kind=kint) ::     MID_ITR =   1
        integer(kind=kint) ::     MIN_ITR =  30
        real(kind=kreal) ::       EPS_MG =         1.0d-8
      end type MGCG_parameter
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine set_MGCG_parameter(MG_ctl, MG_param)
!
      use m_machine_parameter
      use t_ctl_data_4_Multigrid
!
      type(MGCG_control), intent(in) :: MG_ctl
      type(MGCG_parameter), intent(inout) :: MG_param
!
!
      if (MG_ctl%MG_METHOD_ctl%iflag .gt. 0) then
        MG_param%METHOD_MG =     MG_ctl%MG_METHOD_ctl%charavalue
      end if
!
      if (MG_ctl%MG_PRECOND_ctl%iflag .gt. 0) then
        MG_param%PRECOND_MG =    MG_ctl%MG_PRECOND_ctl%charavalue
      end if
!
      if (MG_ctl%maxiter_mid_ctl%iflag .gt. 0) then
        MG_param%MID_ITR =    MG_ctl%maxiter_mid_ctl%intvalue
      end if
!
      if (MG_ctl%MG_residual_ctl%iflag .gt. 0) then
        MG_param%EPS_MG = MG_ctl%MG_residual_ctl%realvalue
      end if
!
      if (MG_ctl%maxiter_coarsest_ctl%iflag .gt. 0) then
        MG_param%MIN_ITR = MG_ctl%maxiter_coarsest_ctl%intvalue
      end if
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'METHOD_MG:  ', trim(MG_param%METHOD_MG)
        write(*,*) 'PRECOND_MG: ', trim(MG_param%PRECOND_MG)
        write(*,*) 'MID_ITR:    ', MG_param%MID_ITR
        write(*,*) 'MIN_ITR:    ', MG_param%MIN_ITR
        write(*,*) 'EPS_MG:     ', MG_param%EPS_MG
      end if
!
      end subroutine set_MGCG_parameter
!
!  ---------------------------------------------------------------------
!
      end module t_MGCG_parameter
