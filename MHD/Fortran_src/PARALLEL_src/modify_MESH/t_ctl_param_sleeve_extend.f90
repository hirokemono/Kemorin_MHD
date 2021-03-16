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
      use m_constants
      use m_machine_parameter
!
      use t_geometry_data
!
      implicit none
!
      character(len = kchara), parameter                                &
     &                        :: hd_vector_trace = 'vector_trace'
      character(len = kchara), parameter                                &
     &                        :: hd_distance =     'sleeve_length'
      character(len = kchara), parameter                                &
     &                        :: hd_ele_count =    'element_count'
!
      integer(kind = kint), parameter :: iflag_vector_trace = 3
      integer(kind = kint), parameter :: iflag_distance =     2
      integer(kind = kint), parameter :: iflag_ele_count =    1
      integer(kind = kint), parameter :: iflag_turn_off =     0
!
!>      Structure of sleeve extension parameter
      type sleeve_extension_param
!>        Sleeve extension mode flag
        integer(kind = kint) :: iflag_expand = iflag_ele_count
!>        Size of sleeve extension
        real(kind = kreal) ::   dist_max
!
!>        Number of field for reference
        integer(kind = kint) :: num_ref = 0
!>        Filed name for reference
        character(len = kchara), allocatable :: ref_vector_name(:)
!>        Filed ID for reference
        character(len = kchara), allocatable :: i_ref_vector(:)
      end type sleeve_extension_param
!
      private :: alloc_sleeve_ext_ref_vector
      private :: distance_by_trace, distance_by_length
      private :: distance_by_element_num
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_sleeve_ext_ref_vector(sleeve_exp_p)
!
      type(sleeve_extension_param), intent(inout) :: sleeve_exp_p
!
      allocate(sleeve_exp_p%ref_vector_name(sleeve_exp_p%num_ref))
      allocate(sleeve_exp_p%i_ref_vector(sleeve_exp_p%num_ref))
!
      end subroutine alloc_sleeve_ext_ref_vector
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_sleeve_ext_ref_vector(sleeve_exp_p)
!
      type(sleeve_extension_param), intent(inout) :: sleeve_exp_p
!
      deallocate(sleeve_exp_p%ref_vector_name)
      deallocate(sleeve_exp_p%i_ref_vector)
!
      end subroutine dealloc_sleeve_ext_ref_vector
!
!  ---------------------------------------------------------------------
!
      subroutine set_ctl_param_sleeve_extension                         &
     &         (sleeve_ctl, sleeve_exp_p, ierr)
!
      use t_ctl_data_FEM_sleeve_size
      use m_machine_parameter
      use m_error_IDs
      use skip_comment_f
!
      type(FEM_sleeve_control), intent(in) :: sleeve_ctl
!
      type(sleeve_extension_param), intent(inout) :: sleeve_exp_p
      integer(kind = kint), intent(inout) :: ierr
!
      character(len = kchara) :: tmpchara
      integer(kind = kint) :: i
!
      ierr = 0
      sleeve_exp_p%iflag_expand = iflag_turn_off
      sleeve_exp_p%dist_max =     one
      sleeve_exp_p%num_ref =  0
!
      if(sleeve_ctl%sleeve_extension_mode_ctl%iflag .gt. 0) then
        tmpchara = sleeve_ctl%sleeve_extension_mode_ctl%charavalue
        if(cmp_no_case(tmpchara, hd_vector_trace)) then
          sleeve_exp_p%iflag_expand = iflag_vector_trace
        else if(cmp_no_case(tmpchara, hd_distance)) then
          sleeve_exp_p%iflag_expand = iflag_distance
        else if(cmp_no_case(tmpchara, hd_ele_count)) then
          sleeve_exp_p%iflag_expand = iflag_ele_count
        end if
      end if
!
      if(sleeve_exp_p%iflag_expand .eq. iflag_vector_trace              &
     &   .or. sleeve_exp_p%iflag_expand .eq. iflag_distance) then
        if(sleeve_ctl%sleeve_size_ctl%iflag .eq. 0) then
          write(e_message,'(a)') 'Set size for sleeve extension'
          ierr = ierr_mesh
          return
        else
          sleeve_exp_p%dist_max = sleeve_ctl%sleeve_size_ctl%realvalue
        end if
!
      else if(sleeve_exp_p%iflag_expand .eq. iflag_ele_count) then
        if(sleeve_ctl%sleeve_level_ctl%iflag .eq. 0                     &
     &      .and.  sleeve_ctl%sleeve_size_ctl%iflag .eq. 0) then
          write(e_message,'(a)') 'Set size for sleeve extension'
          ierr = ierr_mesh
          return
        else if(sleeve_ctl%sleeve_level_ctl%iflag .gt. 0) then
          sleeve_exp_p%dist_max                                         &
     &          = real(sleeve_ctl%sleeve_level_ctl%intvalue) * 0.9d0
        else if(sleeve_ctl%sleeve_size_ctl%iflag .gt. 0) then
          sleeve_exp_p%dist_max                                         &
     &          = sleeve_ctl%sleeve_size_ctl%realvalue * 0.9d0
        end if
      end if
!
      if(sleeve_exp_p%iflag_expand .eq. iflag_vector_trace) then
        sleeve_exp_p%num_ref = sleeve_ctl%ref_vector_ctl%num
!
        call alloc_sleeve_ext_ref_vector(sleeve_exp_p)
!
        if(sleeve_exp_p%num_ref .le. 0) then
          write(e_message,'(a)') 'Set size for sleeve extension'
          ierr = ierr_mesh
          return
        else
          sleeve_exp_p%ref_vector_name(1:sleeve_exp_p%num_ref)          &
     &        = sleeve_ctl%ref_vector_ctl%c_tbl(1:sleeve_exp_p%num_ref)
        end if
      end if
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'sleeve_exp_p%iflag_expand',                         &
     &            sleeve_exp_p%iflag_expand
        write(*,*) 'sleeve_exp_p%dist_max', sleeve_exp_p%dist_max
        write(*,*) 'sleeve_exp_p%num_ref', sleeve_exp_p%num_ref
        do i = 1, sleeve_exp_p%num_ref
          write(*,*) 'sleeve_exp_p%num_ref', i,                         &
     &              trim(sleeve_exp_p%ref_vector_name(i))
        end do
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
