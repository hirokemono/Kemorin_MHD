!>@file   t_control_params_4_fline.f90
!!@brief  module t_control_params_4_fline
!!
!!@date  Programmed by H.Matsui in Aug. 2011
!
!>@brief control parameters for each field line
!!
!!@verbatim
!!      subroutine alloc_fline_starts_ctl(fln_prm)
!!      subroutine alloc_iflag_fline_used_ele(ele, fln_prm)
!!      subroutine alloc_fline_color_field_param(num_field, fln_prm)
!!        type(element_data), intent(in) :: ele
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!
!!      subroutine dealloc_fline_starts_ctl(fln_prm)
!!      subroutine dealloc_iflag_fline_used_ele(fln_prm)
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!
!!      subroutine check_control_params_fline(fln_prm)
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!@endverbatim
!
      module t_control_params_4_fline
!
      use m_precision
      use t_file_IO_parameter
!
      implicit  none
!
!
      integer(kind = kint), parameter :: id_fline_data_code = 11
!
!        integer(kind = kint) :: num_fline
!
      type fieldline_paramter
!>        File of for field line data file
        type(field_IO_params) :: fline_file_IO
!
!>        Area of seed point
        integer(kind = kint) :: id_fline_seed_type = 0
!>        Direction of field line tracing
        integer(kind = kint) :: id_fline_direction = 0
!>        Distoribution of seed point
        integer(kind = kint) :: id_seed_distribution = 0
!>        Surface group ID for seed points
        integer(kind = kint) :: igrp_start_fline_surf_grp = 0
!
!>        Maximum step length for line tracing
        integer(kind = kint) :: max_line_stepping = 1000
!
!>        Field address for fieldline
        integer(kind = kint) :: ifield_4_fline = 0
!>        Component address for fieldline
        integer(kind = kint) :: icomp_4_fline = 0
!
!>        Field address for fieldline color
        integer(kind = kint) :: ifield_linecolor = 0
!>        Component address for fieldline color
        integer(kind = kint) :: icomp_linecolor = 0
!>        Field name for fieldline color
        character(len = kchara) :: name_color_output
!
!>        number of field for coloring
        integer(kind = kint) :: num_color_fields = 0
!>        number of field for coloring
        integer(kind = kint) :: ntot_color_comp =  0
!>        Field name for fieldline color
        character(len = kchara), allocatable :: color_field_name(:)
!>        Field address for fieldline color
        integer(kind = kint), allocatable :: ifleld_color_field(:)
!>        component address for fieldline color
        integer(kind = kint), allocatable :: icomp_color_field(:)
!>        number of component for fieldline color
        integer(kind = kint), allocatable :: istack_color_field(:)
!>        number of component for fieldline color
        integer(kind = kint), allocatable :: ncomp_color_field(:)
!>        number of component for fieldline color
        integer(kind = kint), allocatable :: ncomp_org_color_field(:)
!
!
!>        Number of element group to use in fieldline
        integer(kind = kint) :: nele_grp_area_fline = 0
!>        Element group list to use in fieldline
        integer(kind = kint), allocatable :: id_ele_grp_area_fline(:)
!>        Element flag to use in fieldline
        integer(kind = kint), allocatable :: iflag_fline_used_ele(:)
!
!>        number of seed points
        integer(kind = kint) :: num_each_field_line = 0
!>        local surface ID for seed points
        integer(kind = kint), allocatable :: id_surf_start_fline(:,:)
!>        global surface ID for seed points
        integer(kind = kint), allocatable                               &
     &                       :: id_gl_surf_start_fline(:,:)
!>        outward flux flag
        integer(kind = kint), allocatable                               &
     &                       :: iflag_outward_flux_fline(:)
!>        Position list of seed point
        real(kind = kreal), allocatable :: xx_surf_start_fline(:,:)
!>        Position list of seed point in start element
        real(kind = kreal), allocatable :: xi_surf_start_fline(:,:)
!>        domain list of seed point
        integer(kind = kint), allocatable :: ip_surf_start_fline(:)
!>        element list of seed point
        integer(kind = kint), allocatable :: iele_surf_start_fline(:)
      end type fieldline_paramter
!
!
      integer(kind = kint), parameter :: iflag_surface_group =   0
      integer(kind = kint), parameter :: iflag_surface_list =    1
      integer(kind = kint), parameter :: iflag_position_list =   2
      integer(kind = kint), parameter :: iflag_spray_in_domain = 3
!
!
      integer(kind = kint), parameter :: iflag_backward_trace = -1
      integer(kind = kint), parameter :: iflag_both_trace =      0
      integer(kind = kint), parameter :: iflag_forward_trace =   1
!
      integer(kind = kint), parameter :: iflag_random_by_amp =   0
      integer(kind = kint), parameter :: iflag_random_by_area =  1
      integer(kind = kint), parameter :: iflag_no_random =       2
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_fline_starts_ctl(fln_prm)
!
      type(fieldline_paramter), intent(inout) :: fln_prm
!
      integer(kind = kint) :: num
!
!
      num = fln_prm%nele_grp_area_fline
      allocate(fln_prm%id_ele_grp_area_fline(num))
      if(num .gt. 0) fln_prm%id_ele_grp_area_fline = 0
!
      num = fln_prm%num_each_field_line
      allocate(fln_prm%id_surf_start_fline(2,num))
      allocate(fln_prm%id_gl_surf_start_fline(2,num))
      allocate(fln_prm%iflag_outward_flux_fline(num))
!
      allocate(fln_prm%xx_surf_start_fline(3,num))
      allocate(fln_prm%xi_surf_start_fline(3,num))
      allocate(fln_prm%ip_surf_start_fline(num))
      allocate(fln_prm%iele_surf_start_fline(num))
!
      if(num .gt. 0) then
        fln_prm%id_surf_start_fline(1:2,1:num) =    0
        fln_prm%id_gl_surf_start_fline(1:2,1:num) = 0
        fln_prm%iflag_outward_flux_fline(1:num) =   0
        fln_prm%xx_surf_start_fline(1:3,1:num) =    0.0d0
!
        fln_prm%xi_surf_start_fline(1:3,1:num) =    0.0d0
        fln_prm%ip_surf_start_fline(1:num) =        0
        fln_prm%iele_surf_start_fline(1:num) =      0
      end if
!
      end subroutine alloc_fline_starts_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_iflag_fline_used_ele(ele, fln_prm)
!
      use t_geometry_data
!
      type(element_data), intent(in) :: ele
      type(fieldline_paramter), intent(inout) :: fln_prm
!
!
      allocate(fln_prm%iflag_fline_used_ele(ele%numele))
!
!$omp parallel workshare
      fln_prm%iflag_fline_used_ele = 0
!$omp end parallel workshare
!
      end subroutine alloc_iflag_fline_used_ele
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_fline_color_field_param(num_field, fln_prm)
!
      integer(kind = kint), intent(in) :: num_field
      type(fieldline_paramter), intent(inout) :: fln_prm
!
      fln_prm%num_color_fields = num_field
      allocate(fln_prm%color_field_name(fln_prm%num_color_fields))
      allocate(fln_prm%ifleld_color_field(fln_prm%num_color_fields))
      allocate(fln_prm%icomp_color_field(fln_prm%num_color_fields))
      allocate(fln_prm%istack_color_field(0:fln_prm%num_color_fields))
      allocate(fln_prm%ncomp_color_field(fln_prm%num_color_fields))
      allocate(fln_prm%ncomp_org_color_field(fln_prm%num_color_fields))
!
      end subroutine alloc_fline_color_field_param
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_fline_starts_ctl(fln_prm)
!
      type(fieldline_paramter), intent(inout) :: fln_prm
!
!
      deallocate(fln_prm%id_ele_grp_area_fline)
!
      deallocate(fln_prm%id_surf_start_fline)
      deallocate(fln_prm%id_gl_surf_start_fline)
      deallocate(fln_prm%iflag_outward_flux_fline)

      deallocate(fln_prm%xx_surf_start_fline)
      deallocate(fln_prm%xi_surf_start_fline)
      deallocate(fln_prm%ip_surf_start_fline)
      deallocate(fln_prm%iele_surf_start_fline)
!
      end subroutine dealloc_fline_starts_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_iflag_fline_used_ele(fln_prm)
!
      type(fieldline_paramter), intent(inout) :: fln_prm
!
!
      deallocate(fln_prm%iflag_fline_used_ele)
!
      end subroutine dealloc_iflag_fline_used_ele
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_fline_color_field_param(fln_prm)
!
      type(fieldline_paramter), intent(inout) :: fln_prm
!
      deallocate(fln_prm%color_field_name)
      deallocate(fln_prm%ifleld_color_field)
      deallocate(fln_prm%icomp_color_field)
      deallocate(fln_prm%istack_color_field)
      deallocate(fln_prm%ncomp_color_field)
      deallocate(fln_prm%ncomp_org_color_field)
!
      end subroutine dealloc_fline_color_field_param
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_control_params_fline(fln_prm)
!
      type(fieldline_paramter), intent(in) :: fln_prm
!
      integer(kind = kint) :: i
!
!
        write(*,*) 'fline_header: ',                                    &
     &            trim(fln_prm%fline_file_IO%file_prefix)
        write(*,*) 'file format: ', fln_prm%fline_file_IO%iflag_format
        write(*,*) 'id_fline_direction: ', fln_prm%id_fline_direction
        write(*,*) 'id_fline_seed_type: ', fln_prm%id_fline_seed_type
        write(*,*) 'id_seed_distribution: ',                            &
     &            fln_prm%id_seed_distribution
        write(*,*) 'max_line_stepping: ', fln_prm%max_line_stepping
!
        write(*,*) 'ifield_4_fline: ', fln_prm%ifield_4_fline
        write(*,*) 'icomp_4_fline: ',  fln_prm%icomp_4_fline
        write(*,*) 'ifield_linecolor: ', fln_prm%ifield_linecolor
        write(*,*) 'icomp_linecolor: ', fln_prm%icomp_linecolor
        write(*,*) 'name_color_output: ',                               &
     &            trim(fln_prm%name_color_output)
!
        write(*,*) 'nele_grp_area_fline: ',                             &
     &            fln_prm%nele_grp_area_fline
!
        write(*,*) 'num_each_field_line: ',                             &
     &            fln_prm%num_each_field_line
        if     (fln_prm%id_fline_seed_type                              &
     &                          .eq. iflag_surface_group) then
          write(*,*) 'igrp_start_fline_surf_grp: ',                     &
     &              fln_prm%igrp_start_fline_surf_grp
        else if(fln_prm%id_fline_seed_type                              &
     &                          .eq. iflag_surface_list) then
          do i = 1, fln_prm%num_each_field_line
            write(*,*) i, fln_prm%id_gl_surf_start_fline(1:2,i)
          end do
        else if(fln_prm%id_fline_seed_type                              &
     &                          .eq. iflag_position_list) then
          do i = 1, fln_prm%num_each_field_line
            write(*,*) i, fln_prm%xx_surf_start_fline(1:3,i)
          end do
        end if
!
      end subroutine check_control_params_fline
!
!  ---------------------------------------------------------------------
!
      end module t_control_params_4_fline
