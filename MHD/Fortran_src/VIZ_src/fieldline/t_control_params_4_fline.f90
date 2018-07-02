!t_control_params_4_fline.f90
!      module t_control_params_4_fline
!
!        programmed by H.Matsui on Aug. 2011
!
!!      subroutine alloc_control_params_fline(num_fline, fline_prm)
!!      subroutine alloc_fline_starts_ctl(num_fline, fline_prm)
!!      subroutine alloc_iflag_fline_used_ele                           &
!!     &         (num_fline, ele, fline_prm)
!!        type(element_data), intent(in) :: ele
!!      subroutine dealloc_control_params_fline(fline_prm)
!!      subroutine dealloc_fline_starts_ctl(fline_prm)
!!      subroutine dealloc_iflag_fline_used_ele(fline_prm)
!!
!!      subroutine check_control_params_fline(i_fln)
!
      module t_control_params_4_fline
!
      use m_precision
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
        character(len = kchara) :: fline_prefix
!>        File format for field line data file
        integer(kind = kint) :: iformat_file_file = 0
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
!>        Field address for fieldline color
        integer(kind = kint) :: ifield_linecolor = 0
!>        Component address for fieldline color
        integer(kind = kint) :: icomp_linecolor = 0
!>        Field name for fieldline color
        character(len = kchara) :: name_color_output
      end type fieldline_paramter
!
      type fieldline_paramters
        integer(kind = kint), allocatable :: nele_grp_area_fline(:)
        integer(kind = kint), allocatable :: istack_grp_area_fline(:)
        integer(kind = kint) :: ntot_ele_grp_area_fline
        integer(kind = kint), allocatable :: id_ele_grp_area_fline(:)
!
        integer(kind = kint), allocatable :: iflag_fline_used_ele(:,:)
!
        integer(kind = kint), allocatable :: num_each_field_line(:)
        integer(kind = kint), allocatable :: istack_each_field_line(:)
        integer(kind = kint) :: ntot_each_field_line
        integer(kind = kint), allocatable :: id_surf_start_fline(:,:)
        integer(kind = kint), allocatable :: id_gl_surf_start_fline(:,:)
        integer(kind = kint), allocatable :: iflag_outward_flux_fline(:)
        real(kind = kreal), allocatable :: xx_surf_start_fline(:,:)
      end type fieldline_paramters
!
!
      character(len = kchara), parameter                                &
     &               :: cflag_surface_group = 'surface_group'
      character(len = kchara), parameter                                &
     &               :: cflag_surface_list =  'surface_list'
      character(len = kchara), parameter                                &
     &               :: cflag_position_list = 'position_list'
      character(len = kchara), parameter                                &
     &               :: cflag_spray_in_domain = 'spray_in_domain'
      integer(kind = kint), parameter :: iflag_surface_group =   0
      integer(kind = kint), parameter :: iflag_surface_list =    1
      integer(kind = kint), parameter :: iflag_position_list =   2
      integer(kind = kint), parameter :: iflag_spray_in_domain = 3
!
      character(len = kchara), parameter                                &
     &               :: cflag_forward_trace =  'forward'
      character(len = kchara), parameter                                &
     &               :: cflag_backward_trace = 'backward'
      character(len = kchara), parameter                                &
     &               :: cflag_both_trace =     'both'
      integer(kind = kint), parameter :: iflag_backward_trace = -1
      integer(kind = kint), parameter :: iflag_both_trace =      0
      integer(kind = kint), parameter :: iflag_forward_trace =   1
!
      character(len = kchara), parameter                                &
     &               :: cflag_random_by_amp =  'amplitude'
      character(len = kchara), parameter                                &
     &               :: cflag_random_by_area = 'area_size'
      character(len = kchara), parameter                                &
     &               :: cflag_no_random =      'no_random'
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
      subroutine alloc_control_params_fline(num_fline, fline_prm)
!
      integer(kind = kint), intent(in) :: num_fline
      type(fieldline_paramters), intent(inout) :: fline_prm
!
!
      allocate(fline_prm%nele_grp_area_fline(num_fline))
      allocate(fline_prm%istack_grp_area_fline(0:num_fline))
!
      allocate(fline_prm%num_each_field_line(num_fline))
      allocate(fline_prm%istack_each_field_line(0:num_fline))
!
      fline_prm%istack_grp_area_fline = 0
      fline_prm%istack_each_field_line = 0
      if(num_fline .gt. 0) then
        fline_prm%nele_grp_area_fline =   0
        fline_prm%num_each_field_line =    0
      end if
!
      end subroutine alloc_control_params_fline
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_fline_starts_ctl(num_fline, fline_prm)
!
      integer(kind = kint), intent(in) :: num_fline
      type(fieldline_paramters), intent(inout) :: fline_prm
      integer(kind = kint) :: num
!
      fline_prm%ntot_ele_grp_area_fline                                 &
     &       = fline_prm%istack_grp_area_fline(num_fline)
      fline_prm%ntot_each_field_line                                    &
     &       = fline_prm%istack_each_field_line(num_fline)
!
      num = fline_prm%ntot_ele_grp_area_fline
      allocate(fline_prm%id_ele_grp_area_fline(num))
!
      num = fline_prm%ntot_each_field_line
      allocate(fline_prm%id_surf_start_fline(2,num))
      allocate(fline_prm%id_gl_surf_start_fline(2,num))
      allocate(fline_prm%xx_surf_start_fline(3,num))
      allocate(fline_prm%iflag_outward_flux_fline(num))
!
      if(num .gt. 0) then
        fline_prm%id_ele_grp_area_fline = 0
        fline_prm%id_surf_start_fline =   0
        fline_prm%id_gl_surf_start_fline = 0
        fline_prm%iflag_outward_flux_fline = 0
        fline_prm%xx_surf_start_fline =   0.0d0
      end if
!
      end subroutine alloc_fline_starts_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_iflag_fline_used_ele                             &
     &         (num_fline, ele, fline_prm)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: num_fline
      type(element_data), intent(in) :: ele
      type(fieldline_paramters), intent(inout) :: fline_prm
!
!
      allocate(fline_prm%iflag_fline_used_ele(ele%numele,num_fline))
      fline_prm%iflag_fline_used_ele = 0
!
      end subroutine alloc_iflag_fline_used_ele
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_control_params_fline(fline_prm)
!
      type(fieldline_paramters), intent(inout) :: fline_prm
!
!
      deallocate(fline_prm%nele_grp_area_fline)
      deallocate(fline_prm%istack_grp_area_fline)
!
      deallocate(fline_prm%num_each_field_line)
      deallocate(fline_prm%istack_each_field_line)
!
      end subroutine dealloc_control_params_fline
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_fline_starts_ctl(fline_prm)
!
      type(fieldline_paramters), intent(inout) :: fline_prm
!
!
      deallocate(fline_prm%id_ele_grp_area_fline)
!
      deallocate(fline_prm%id_surf_start_fline)
      deallocate(fline_prm%id_gl_surf_start_fline)
      deallocate(fline_prm%xx_surf_start_fline)
      deallocate(fline_prm%iflag_outward_flux_fline)
!
      end subroutine dealloc_fline_starts_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_iflag_fline_used_ele(fline_prm)
!
      type(fieldline_paramters), intent(inout) :: fline_prm
!
      deallocate(fline_prm%iflag_fline_used_ele)
!
      end subroutine dealloc_iflag_fline_used_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_control_params_fline(i_fln, fln_prm, fline_prm)
!
      integer(kind = kint), intent(in) :: i_fln
      integer(kind = kint) :: i, ist, ied
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(fieldline_paramters), intent(in) :: fline_prm
!
!
        write(*,*) 'field line parameters for No.', i_fln
        write(*,*) 'fline_header: ', trim(fln_prm%fline_prefix)
        write(*,*) 'file format: ', fln_prm%iformat_file_file
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
        ist = fline_prm%istack_grp_area_fline(i_fln-1) + 1
        ied = fline_prm%istack_grp_area_fline(i_fln  )
        write(*,*) 'nele_grp_area_fline: ',                             &
     &            fline_prm%nele_grp_area_fline(i_fln)
        write(*,*) 'ntot_ele_grp_area_fline: ',                         &
     &            fline_prm%id_ele_grp_area_fline(ist:ied)
!
        write(*,*) 'num_each_field_line: ',                             &
     &            fline_prm%num_each_field_line(i_fln)
        if     (fln_prm%id_fline_seed_type                              &
     &                          .eq. iflag_surface_group) then
          write(*,*) 'igrp_start_fline_surf_grp: ',                     &
     &              fln_prm%igrp_start_fline_surf_grp
        else if(fln_prm%id_fline_seed_type                              &
     &                          .eq. iflag_surface_list) then
          ist = fline_prm%istack_each_field_line(i_fln-1)
          do i = 1, fline_prm%num_each_field_line(i_fln)
            write(*,*) i, fline_prm%id_gl_surf_start_fline(1:2,ist+i)
          end do
        else if(fln_prm%id_fline_seed_type                              &
     &                          .eq. iflag_position_list) then
          ist = fline_prm%istack_each_field_line(i_fln-1)
          do i = 1, fline_prm%num_each_field_line(i_fln)
            write(*,*) i, fline_prm%xx_surf_start_fline(1:3,ist+i)
          end do
        end if
!
      end subroutine check_control_params_fline
!
!  ---------------------------------------------------------------------
!
      end module t_control_params_4_fline
