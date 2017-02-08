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
!
      type FEM_MHD_paremeters
!>          Using rotation form for inertia and Lorentz force
        integer(kind = kint) :: iflag_rotate_form =  id_turn_OFF
!>         Coriolist terms adjustment in implicit scheme
        integer(kind = kint) :: iflag_imp_correct = id_turn_OFF
      end type FEM_MHD_paremeters
!
      end module t_FEM_control_parameter
