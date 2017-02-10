!>@file   t_FEM_control_parameter.f90
!!@brief  module t_FEM_control_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!!@brief  module t_FEM_control_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!> @brief control flags for FEM MHD dynamo model
!!
!!@verbatim
!!      subroutine alloc_filter_group_param(num_grp, f_area)
!!      subroutine dealloc_SGS_filter_groups(filter_param)
!!      subroutine dealloc_filter_group_param(f_area)
!!
!!      subroutine copy_filter_group_param(f_area_org, f_area_new)
!!        type(SGS_filter_area_params), intent(in) :: f_area_org
!!        type(SGS_filter_area_params), intent(inout) :: f_area_new
!!@endverbatim
!
      module t_FEM_control_parameter
!
      use m_precision
      use m_constants
!
      implicit  none
!
      character(len=kchara), parameter                                  &
     &                      :: cflag_rot_form = 'Rotation_form'
!
!>      ID for using SUPG by mangeitc field
      integer (kind=kint), parameter :: id_magnetic_SUPG =  2
!
      type FEM_MHD_paremeters
!>        Number of quadrature points for time evolution
        integer(kind = kint)  :: npoint_t_evo_int =   2
!FEM_prm%npint_t_evo_int
!
!>        Number of iteration for Multi-pass scheme
        integer(kind = kint)  :: num_multi_pass =     0
!
!>          Using rotation form for inertia and Lorentz force
        integer(kind = kint) :: iflag_rotate_form =  id_turn_OFF
!>         Coriolist terms adjustment in implicit scheme
        integer(kind = kint) :: iflag_imp_correct = id_turn_OFF
!
!>        SUPG flag for velocity
        integer (kind=kint) :: iflag_velo_supg =  id_turn_OFF
!>        SUPG flag for magnetic field
        integer (kind=kint) :: iflag_magne_supg = id_turn_OFF
!>        SUPG flag for temperature
        integer (kind=kint) :: iflag_temp_supg = id_turn_OFF
!>        SUPG flag for light element
        integer (kind=kint) :: iflag_comp_supg = id_turn_OFF
      end type FEM_MHD_paremeters
!
      end module t_FEM_control_parameter
