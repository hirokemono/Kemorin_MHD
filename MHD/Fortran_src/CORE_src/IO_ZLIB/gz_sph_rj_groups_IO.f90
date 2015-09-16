!gz_sph_rj_groups_IO.f90
!      module gz_sph_rj_groups_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_modes_rj_groups_gz
!      subroutine write_modes_rj_groups_gz
!
      module gz_sph_rj_groups_IO
!
      use m_precision
!
      use m_group_data_sph_specr_IO
      use gz_group_data_IO
      use skip_gz_comment
!
      implicit none
!
      private :: read_rj_radial_grp_data_gz
      private :: read_rj_sphere_grp_data_gz
      private :: write_rj_radial_grp_data_gz
      private :: write_rj_sphere_grp_data_gz
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_modes_rj_groups_gz
!
!
      call read_rj_radial_grp_data_gz
      call read_rj_sphere_grp_data_gz
!
      end subroutine read_modes_rj_groups_gz
!
!------------------------------------------------------------------
!
      subroutine write_modes_rj_groups_gz
!
      use m_sph_modes_grid_labels
!
!
      textbuf = hd_grphd() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_rj_radial_grp_data_gz
      call write_rj_sphere_grp_data_gz
!
      end subroutine write_modes_rj_groups_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_rj_radial_grp_data_gz
!
!
      call skip_gz_comment_int(num_radial_grp_rj_IO)
      call allocate_rj_r_grp_IO_stack
!
      if (num_radial_grp_rj_IO .gt. 0) then
        call read_group_stack_gz(num_radial_grp_rj_IO,                  &
     &      ntot_radial_grp_rj_IO, istack_radial_grp_rj_IO)
!
        call allocate_rj_r_grp_IO_item
        call read_group_item_gz(num_radial_grp_rj_IO,                   &
     &      ntot_radial_grp_rj_IO, istack_radial_grp_rj_IO,             &
     &      name_radial_grp_rj_IO,item_radial_grp_rj_IO)
!
      else
        ntot_radial_grp_rj_IO = 0
        call allocate_rj_r_grp_IO_item
      end if
!
      end subroutine read_rj_radial_grp_data_gz
!
!------------------------------------------------------------------
!
      subroutine read_rj_sphere_grp_data_gz
!
!
      call skip_gz_comment_int(num_sphere_grp_rj_IO)
      call allocate_rj_j_grp_IO_stack
!
      if (num_sphere_grp_rj_IO .gt. 0) then
        call read_group_stack_gz(num_sphere_grp_rj_IO,                  &
     &      ntot_sphere_grp_rj_IO, istack_sphere_grp_rj_IO)
!
        call allocate_rj_j_grp_IO_item
        call read_group_item_gz(num_sphere_grp_rj_IO,                   &
     &      ntot_sphere_grp_rj_IO, istack_sphere_grp_rj_IO,             &
     &      name_sphere_grp_rj_IO, item_sphere_grp_rj_IO)
!
      else
        ntot_sphere_grp_rj_IO = 0
        call allocate_rj_j_grp_IO_item
      end if
!
      end subroutine read_rj_sphere_grp_data_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_rj_radial_grp_data_gz
!
      use m_sph_modes_grid_labels
!
!
      textbuf = hd_kgrphd() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_group_data_gz(num_radial_grp_rj_IO,                    &
     &    ntot_radial_grp_rj_IO, istack_radial_grp_rj_IO,               &
     &    name_radial_grp_rj_IO, item_radial_grp_rj_IO)
!
      call deallocate_rj_r_grp_IO_item
!
      end subroutine write_rj_radial_grp_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_rj_sphere_grp_data_gz
!
      use m_sph_modes_grid_labels
!
!
      textbuf = hd_jgrphd() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_group_data_gz(num_sphere_grp_rj_IO,                    &
     &    ntot_sphere_grp_rj_IO, istack_sphere_grp_rj_IO,               &
     &    name_sphere_grp_rj_IO, item_sphere_grp_rj_IO)
!
      call deallocate_rj_j_grp_IO_item
!
      end subroutine write_rj_sphere_grp_data_gz
!
!------------------------------------------------------------------
!
      end module gz_sph_rj_groups_IO
