!> @file  t_ctl_param_sleeve_extend.f90
!!      module t_ctl_param_sleeve_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Control parameter for sleeve extension
!!
!!@verbatim
!!      subroutine set_ctl_param_sleeve_extension                       &
!!     &         (ext_mode_ctl, ext_size_ctl, sleeve_exp_p, ierr)
!!        type(read_character_item), intent(in) :: ext_mode_ctl
!!        type(read_real_item), intent(in) ::      ext_size_ctl
!!        type(sleeve_extension_param), intent(inout) :: sleeve_exp_p
!!
!!      real(kind = kreal) function distance_select(sleeve_exp_p, i, j, &
!!     &                                            node, vect)
!!        integer(kind = kint), intent(in) :: i, j
!!        type(sleeve_extension_param), intent(in) :: sleeve_exp_p
!!        type(node_data), intent(in) :: node
!!        real(kind = kreal), intent(in) :: dist_max
!!        real(kind = kreal), intent(in) :: vect(node%numnod,3)
!!@endverbatim
!
      module t_ctl_param_sleeve_extend
!
      use m_precision
!
      use t_geometry_data
!
      character(len = kchara), parameter                                &
     &                        :: hd_vector_trace = 'vector_trace'
      character(len = kchara), parameter                                &
     &                        :: hd_distance =     'element_length'
      character(len = kchara), parameter                                &
     &                        :: hd_ele_count =    'element_count'
!
      integer(kind = kint), parameter :: iflag_vector_trace = 3
      integer(kind = kint), parameter :: iflag_distance =     2
      integer(kind = kint), parameter :: iflag_ele_count =    1
      integer(kind = kint), parameter :: iflag_turn_off =     0
!
      type sleeve_extension_param
        integer(kind = kint) :: iflag_expand = iflag_ele_count
        real(kind = kreal) ::   dist_max
      end type sleeve_extension_param
!
      private :: distance_by_trace, distance_by_length
      private :: distance_by_element_num
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_ctl_param_sleeve_extension                         &
     &         (ext_mode_ctl, ext_size_ctl, sleeve_exp_p, ierr)
!
      use m_machine_parameter
      use m_error_IDs
      use t_control_array_character
      use t_control_array_real
      use skip_comment_f
!
      type(read_character_item), intent(in) :: ext_mode_ctl
      type(read_real_item), intent(in) ::      ext_size_ctl
!
      type(sleeve_extension_param), intent(inout) :: sleeve_exp_p
      integer(kind = kint), intent(inout) :: ierr
!
      character(len = kchara) :: tmpchara
!
      ierr = 0
      sleeve_exp_p%iflag_expand = iflag_turn_off
      sleeve_exp_p%dist_max =     one
!
      if(ext_mode_ctl%iflag .gt. 0) then
        tmpchara = ext_mode_ctl%charavalue
        if(cmp_no_case(tmpchara, hd_vector_trace)) then
          sleeve_exp_p%iflag_expand = iflag_vector_trace
        else if(cmp_no_case(tmpchara, hd_distance)) then
          sleeve_exp_p%iflag_expand = iflag_distance
        else if(cmp_no_case(tmpchara, hd_ele_count)) then
          sleeve_exp_p%iflag_expand = iflag_ele_count
        end if
      end if
!
      if(ext_size_ctl%iflag .eq. 0) then
        if(sleeve_exp_p%iflag_expand .ne. 0) then
          write(e_message,'(a)') 'Set size for sleeve extension'
          ierr = ierr_mesh
          return
        end if
      else
        sleeve_exp_p%dist_max = ext_size_ctl%realvalue
      end if
!
      end subroutine set_ctl_param_sleeve_extension
!
!  ---------------------------------------------------------------------
!
      real(kind = kreal) function distance_select(sleeve_exp_p, i, j,   &
     &                                            node, vect)
!
      integer(kind = kint), intent(in) :: i, j
      type(sleeve_extension_param), intent(in) :: sleeve_exp_p
      type(node_data), intent(in) :: node
      real(kind = kreal), intent(in) :: vect(node%numnod,3)
!
!
      if(sleeve_exp_p%iflag_expand .eq. iflag_vector_trace) then
        distance_select = distance_by_trace(sleeve_exp_p%dist_max,      &
     &                                      i, j, node, vect)
      else if(sleeve_exp_p%iflag_expand .eq. iflag_distance) then
        distance_select = distance_by_length(i, j, node)
      else
        distance_select = distance_by_element_num()
      end if
!
      end function distance_select
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      real(kind = kreal) function distance_by_trace(dist_max, i, j,     &
     &                                              node, vect)
!
      integer(kind = kint), intent(in) :: i, j
      type(node_data), intent(in) :: node
      real(kind = kreal), intent(in) :: dist_max
      real(kind = kreal), intent(in) :: vect(node%numnod,3)
!
      real(kind = kreal) :: prod, dist
!
!
      prod =  vect(i,1) * (node%xx(j,1) - node%xx(i,1))                 &
     &      + vect(i,2) * (node%xx(j,2) - node%xx(i,2))                 &
     &      + vect(i,3) * (node%xx(j,3) - node%xx(i,3))
      if(prod .eq. 0.0d0) then
        dist = dist_max
      else
        dist = sqrt(vect(i,1)**2 + vect(i,2)**2 + vect(i,3)**2)         &
     &        * ((node%xx(j,1) - node%xx(i,1))**2                       &
     &         + (node%xx(j,2) - node%xx(i,2))**2                       &
     &         + (node%xx(j,3) - node%xx(i,3))**2) / prod
      end if
      distance_by_trace = abs(dist)
!
      end function distance_by_trace
!
!  ---------------------------------------------------------------------
!
      real(kind = kreal) function distance_by_length(i, j, node)
!
      integer(kind = kint), intent(in) :: i, j
      type(node_data), intent(in) :: node
!
      distance_by_length = sqrt((node%xx(j,1) - node%xx(i,1))**2        &
     &                        + (node%xx(j,2) - node%xx(i,2))**2        &
     &                        + (node%xx(j,3) - node%xx(i,3))**2)
!
      end function distance_by_length
!
!  ---------------------------------------------------------------------
!
      real(kind = kreal) function distance_by_element_num()
!
      distance_by_element_num = 1.0d0
!
      end function distance_by_element_num
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_param_sleeve_extend
