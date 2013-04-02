!m_control_params_4_fline.f90
!      module m_control_params_4_fline
!
      module m_control_params_4_fline
!
!        programmed by H.Matsui on Aug. 2011
!
      use m_precision
!
      implicit  none
!
!
      integer(kind = kint), parameter :: fline_ctl_file_code = 11
      integer(kind = kint), parameter :: id_fline_data_code = 11
!
      integer(kind = kint) :: num_fline
      character(len = kchara), allocatable :: fline_header(:)
      character(len = kchara) :: fname_fline_out
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
!
!      subroutine allocate_control_params_fline
!      subroutine allocate_fline_starts_ctl
!      subroutine allocate_iflag_fline_used_ele
!      subroutine deallocate_iflag_fline_used_ele
!
!      subroutine check_control_params_fline(i_fln)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_control_params_fline
!
!
      allocate(fline_header(num_fline))
      allocate(id_fline_file_type(num_fline))
!
      allocate(id_fline_start_type(num_fline))
      allocate(id_fline_direction(num_fline))
      allocate(id_fline_start_dist(num_fline))
      allocate(max_line_stepping(num_fline))
!
      allocate(nele_grp_area_fline(num_fline))
      allocate(istack_grp_area_fline(0:num_fline))
!
      allocate(num_each_field_line(num_fline))
      allocate(istack_each_field_line(0:num_fline))
!
      allocate(igrp_start_fline_surf_grp(num_fline))
!
      id_fline_file_type =   0
      id_fline_start_type =  0
      id_fline_direction =   0
      id_fline_start_dist =  0
!
      nele_grp_area_fline =   0
      istack_grp_area_fline = 0
      num_each_field_line =    0
      istack_each_field_line = 0
      igrp_start_fline_surf_grp = 0
      max_line_stepping = 0
!
      allocate(ifield_4_fline(num_fline)   )
      allocate(icomp_4_fline(num_fline)    )
      allocate(ifield_linecolor(num_fline) )
      allocate(icomp_linecolor(num_fline) )
      allocate(name_color_output(num_fline) )
      ifield_4_fline =     0
      icomp_4_fline =      0
      ifield_linecolor =   0
      icomp_linecolor =    0
!
      end subroutine allocate_control_params_fline
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_fline_starts_ctl
!
      ntot_ele_grp_area_fline = istack_grp_area_fline(num_fline)
      ntot_each_field_line =    istack_each_field_line(num_fline)
!
      allocate(id_ele_grp_area_fline(ntot_ele_grp_area_fline))
!
      allocate(id_surf_start_fline(2,ntot_each_field_line))
      allocate(id_gl_surf_start_fline(2,ntot_each_field_line))
      allocate(xx_surf_start_fline(3,ntot_each_field_line))
      allocate(iflag_outward_flux_fline(ntot_each_field_line))
!
      id_ele_grp_area_fline = 0
      id_surf_start_fline =   0
      id_gl_surf_start_fline = 0
      iflag_outward_flux_fline = 0
      xx_surf_start_fline =   0.0d0
!
      end subroutine allocate_fline_starts_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_iflag_fline_used_ele(numele)
!
      integer(kind = kint), intent(in) :: numele
!
!
      allocate(iflag_fline_used_ele(numele,num_fline))
      iflag_fline_used_ele = 0
!
      end subroutine allocate_iflag_fline_used_ele
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_iflag_fline_used_ele
!
      deallocate(iflag_fline_used_ele)
!
      end subroutine deallocate_iflag_fline_used_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_control_params_fline(i_fln)
!
      integer(kind = kint), intent(in) :: i_fln
      integer(kind = kint) :: i, ist, ied
!
!
        write(*,*) 'field line parameters for No.', i_fln
        write(*,*) 'fline_header: ', trim(fline_header(i_fln))
        write(*,*) 'id_fline_file_type: ', id_fline_file_type(i_fln)
        write(*,*) 'id_fline_direction: ', id_fline_direction(i_fln)
        write(*,*) 'id_fline_start_type: ', id_fline_start_type(i_fln)
        write(*,*) 'id_fline_start_dist: ', id_fline_start_dist(i_fln)
        write(*,*) 'max_line_stepping: ',   max_line_stepping(i_fln)
!
        write(*,*) 'ifield_4_fline: ', ifield_4_fline(i_fln)
        write(*,*) 'icomp_4_fline: ',  icomp_4_fline(i_fln)
        write(*,*) 'ifield_linecolor: ', ifield_linecolor(i_fln)
        write(*,*) 'icomp_linecolor: ', icomp_linecolor(i_fln)
        write(*,*) 'name_color_output: ',trim(name_color_output(i_fln))
!
        ist = istack_grp_area_fline(i_fln-1) + 1
        ied = istack_grp_area_fline(i_fln  )
        write(*,*) 'nele_grp_area_fline: ', nele_grp_area_fline(i_fln)
        write(*,*) 'ntot_ele_grp_area_fline: ',                         &
     &            id_ele_grp_area_fline(ist:ied)
!
        write(*,*) 'num_each_field_line: ', num_each_field_line(i_fln)
        if     (id_fline_start_type(i_fln) .eq. 0) then
          write(*,*) 'igrp_start_fline_surf_grp: ',                     &
     &              igrp_start_fline_surf_grp(i_fln)
        else if(id_fline_start_type(i_fln) .eq. 1) then
          ist = istack_each_field_line(i_fln-1)
          do i = 1, num_each_field_line(i_fln)
            write(*,*) i, id_gl_surf_start_fline(1:2,ist+i)
          end do
        else if(id_fline_start_type(i_fln) .eq. 2) then
          ist = istack_each_field_line(i_fln-1)
          do i = 1, num_each_field_line(i_fln)
            write(*,*) i, xx_surf_start_fline(1:3,ist+i)
          end do
        end if
!
      end subroutine check_control_params_fline
!
!  ---------------------------------------------------------------------
!
      end module m_control_params_4_fline
