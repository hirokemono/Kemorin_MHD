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
      type fieldline_paramters
        character(len = kchara), allocatable :: fline_header(:)
!
        integer(kind = kint), allocatable :: id_fline_file_type(:)
!
        integer(kind = kint), allocatable :: id_fline_start_type(:)
        integer(kind = kint), allocatable :: id_fline_direction(:)
        integer(kind = kint), allocatable :: id_fline_start_dist(:)
        integer(kind = kint), allocatable :: max_line_stepping(:)
!
        integer(kind = kint), allocatable :: nele_grp_area_fline(:)
        integer(kind = kint), allocatable :: istack_grp_area_fline(:)
        integer(kind = kint) :: ntot_ele_grp_area_fline
        integer(kind = kint), allocatable :: id_ele_grp_area_fline(:)
!
        integer(kind = kint), allocatable :: iflag_fline_used_ele(:,:)
!
        integer(kind = kint), allocatable :: igrp_start_fline_surf_grp(:)
!
        integer(kind = kint), allocatable :: num_each_field_line(:)
        integer(kind = kint), allocatable :: istack_each_field_line(:)
        integer(kind = kint) :: ntot_each_field_line
        integer(kind = kint), allocatable :: id_surf_start_fline(:,:)
        integer(kind = kint), allocatable :: id_gl_surf_start_fline(:,:)
        integer(kind = kint), allocatable :: iflag_outward_flux_fline(:)
        real(kind = kreal), allocatable :: xx_surf_start_fline(:,:)
!
        integer(kind = kint), allocatable :: ifield_4_fline(:)
        integer(kind = kint), allocatable :: icomp_4_fline(:)
        integer(kind = kint), allocatable :: ifield_linecolor(:)
        integer(kind = kint), allocatable :: icomp_linecolor(:)
        character(len = kchara), allocatable :: name_color_output(:)
      end type fieldline_paramters
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
      allocate(fline_prm%fline_header(num_fline))
      allocate(fline_prm%id_fline_file_type(num_fline))
!
      allocate(fline_prm%id_fline_start_type(num_fline))
      allocate(fline_prm%id_fline_direction(num_fline))
      allocate(fline_prm%id_fline_start_dist(num_fline))
      allocate(fline_prm%max_line_stepping(num_fline))
!
      allocate(fline_prm%nele_grp_area_fline(num_fline))
      allocate(fline_prm%istack_grp_area_fline(0:num_fline))
!
      allocate(fline_prm%num_each_field_line(num_fline))
      allocate(fline_prm%istack_each_field_line(0:num_fline))
!
      allocate(fline_prm%igrp_start_fline_surf_grp(num_fline))
!
      fline_prm%istack_grp_area_fline = 0
      fline_prm%istack_each_field_line = 0
      if(num_fline .gt. 0) then
        fline_prm%id_fline_file_type =   0
        fline_prm%id_fline_start_type =  0
        fline_prm%id_fline_direction =   0
        fline_prm%id_fline_start_dist =  0
!
        fline_prm%nele_grp_area_fline =   0
        fline_prm%num_each_field_line =    0
        fline_prm%igrp_start_fline_surf_grp = 0
        fline_prm%max_line_stepping = 0
      end if
!
      allocate(fline_prm%ifield_4_fline(num_fline)   )
      allocate(fline_prm%icomp_4_fline(num_fline)    )
      allocate(fline_prm%ifield_linecolor(num_fline) )
      allocate(fline_prm%icomp_linecolor(num_fline) )
      allocate(fline_prm%name_color_output(num_fline) )
!
      if(num_fline .gt. 0) then
        fline_prm%ifield_4_fline =     0
        fline_prm%icomp_4_fline =      0
        fline_prm%ifield_linecolor =   0
        fline_prm%icomp_linecolor =    0
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
      deallocate(fline_prm%fline_header)
      deallocate(fline_prm%id_fline_file_type)
!
      deallocate(fline_prm%id_fline_start_type)
      deallocate(fline_prm%id_fline_direction)
      deallocate(fline_prm%id_fline_start_dist)
      deallocate(fline_prm%max_line_stepping)
!
      deallocate(fline_prm%nele_grp_area_fline)
      deallocate(fline_prm%istack_grp_area_fline)
!
      deallocate(fline_prm%num_each_field_line)
      deallocate(fline_prm%istack_each_field_line)
!
      deallocate(fline_prm%igrp_start_fline_surf_grp)
!
      deallocate(fline_prm%ifield_4_fline   )
      deallocate(fline_prm%icomp_4_fline    )
      deallocate(fline_prm%ifield_linecolor )
      deallocate(fline_prm%icomp_linecolor )
      deallocate(fline_prm%name_color_output )
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
      subroutine check_control_params_fline(i_fln, fline_prm)
!
      integer(kind = kint), intent(in) :: i_fln
      integer(kind = kint) :: i, ist, ied
!
      type(fieldline_paramters), intent(in) :: fline_prm
!
!
        write(*,*) 'field line parameters for No.', i_fln
        write(*,*) 'fline_header: ',                                    &
     &            trim(fline_prm%fline_header(i_fln))
        write(*,*) 'id_fline_file_type: ',                              &
     &            fline_prm%id_fline_file_type(i_fln)
        write(*,*) 'id_fline_direction: ',                              &
     &            fline_prm%id_fline_direction(i_fln)
        write(*,*) 'id_fline_start_type: ',                             &
     &            fline_prm%id_fline_start_type(i_fln)
        write(*,*) 'id_fline_start_dist: ',                             &
     &            fline_prm%id_fline_start_dist(i_fln)
        write(*,*) 'max_line_stepping: ',                               &
     &            fline_prm%max_line_stepping(i_fln)
!
        write(*,*) 'ifield_4_fline: ', fline_prm%ifield_4_fline(i_fln)
        write(*,*) 'icomp_4_fline: ',  fline_prm%icomp_4_fline(i_fln)
        write(*,*) 'ifield_linecolor: ',                                &
     &            fline_prm%ifield_linecolor(i_fln)
        write(*,*) 'icomp_linecolor: ',                                 &
     &            fline_prm%icomp_linecolor(i_fln)
        write(*,*) 'name_color_output: ',                               &
     &            trim(fline_prm%name_color_output(i_fln))
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
        if     (fline_prm%id_fline_start_type(i_fln) .eq. 0) then
          write(*,*) 'igrp_start_fline_surf_grp: ',                     &
     &              fline_prm%igrp_start_fline_surf_grp(i_fln)
        else if(fline_prm%id_fline_start_type(i_fln) .eq. 1) then
          ist = fline_prm%istack_each_field_line(i_fln-1)
          do i = 1, fline_prm%num_each_field_line(i_fln)
            write(*,*) i, fline_prm%id_gl_surf_start_fline(1:2,ist+i)
          end do
        else if(fline_prm%id_fline_start_type(i_fln) .eq. 2) then
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
