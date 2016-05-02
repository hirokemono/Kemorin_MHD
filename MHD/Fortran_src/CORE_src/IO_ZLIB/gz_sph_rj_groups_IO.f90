!gz_sph_rj_groups_IO.f90
!      module gz_sph_rj_groups_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_modes_rj_groups_gz
!      subroutine read_geom_rtp_groups_gz
!      subroutine write_modes_rj_groups_gz
!      subroutine write_geom_rtp_groups_gz
!
      module gz_sph_rj_groups_IO
!
      use m_precision
!
      use gz_group_data_IO
      use skip_gz_comment
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_modes_rj_groups_gz
!
      use m_group_data_sph_specr_IO
!
!
      call read_rj_grp_data_gz(radial_rj_grp_IO)
      call read_rj_grp_data_gz(sphere_rj_grp_IO)
!
      end subroutine read_modes_rj_groups_gz
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtp_groups_gz
!
      use m_group_data_sph_specr_IO
!
!
!      write(*,*) 'read_rtp_node_grp_data_gz'
      call read_rj_grp_data_gz(bc_rtp_grp_IO)
!      write(*,*) 'read_rtp_radial_grp_data_gz'
      call read_rj_grp_data_gz(radial_rtp_grp_IO)
!      write(*,*) 'read_rtp_theta_grp_data_gz'
      call read_rj_grp_data_gz(theta_rtp_grp_IO)
!      write(*,*) 'read_rtp_zonal_grp_data_gz'
      call read_rj_grp_data_gz(zonal_rtp_grp_IO)
!
      end subroutine read_geom_rtp_groups_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_modes_rj_groups_gz
!
      use m_sph_modes_grid_labels
      use m_group_data_sph_specr_IO
!
!
      textbuf = hd_grphd() // char(0)
      call gz_write_textbuf_no_lf
!
      textbuf = hd_kgrphd() // char(0)
      call gz_write_textbuf_no_lf
      call write_rj_grp_data_gz(radial_rj_grp_IO)
!
      textbuf = hd_jgrphd() // char(0)
      call gz_write_textbuf_no_lf
      call write_rj_grp_data_gz(sphere_rj_grp_IO)
!
      end subroutine write_modes_rj_groups_gz
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_groups_gz
!
      use m_sph_modes_grid_labels
      use m_group_data_sph_specr_IO
!
!
      textbuf = hd_grphd() // char(0)
      call gz_write_textbuf_no_lf
!
      textbuf = hd_ngrphd() // char(0)
      call gz_write_textbuf_no_lf
      call write_rj_grp_data_gz(bc_rtp_grp_IO)
!
      textbuf = hd_rgrphd() // char(0)
      call gz_write_textbuf_no_lf
      call write_rj_grp_data_gz(radial_rtp_grp_IO)
!
      textbuf = hd_tgrphd() // char(0)
      call gz_write_textbuf_no_lf
      call write_rj_grp_data_gz(theta_rtp_grp_IO)
!
      textbuf = hd_pgrphd() // char(0)
      call gz_write_textbuf_no_lf
      call write_rj_grp_data_gz(zonal_rtp_grp_IO)
!
      end subroutine write_geom_rtp_groups_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_rj_grp_data_gz(rj_grp_IO)
!
      use t_group_data
!
      type(group_data), intent(inout) :: rj_grp_IO
!
!
      call skip_gz_comment_int(rj_grp_IO%num_grp)
      call allocate_grp_type_num(rj_grp_IO)
!
      if (rj_grp_IO%num_grp .gt. 0) then
        call read_group_stack_gz(rj_grp_IO%num_grp,                     &
     &      rj_grp_IO%num_item, rj_grp_IO%istack_grp)
!
        call allocate_grp_type_item(rj_grp_IO)
        call read_group_item_gz(rj_grp_IO%num_grp,                      &
     &      rj_grp_IO%num_item, rj_grp_IO%istack_grp,                   &
     &      rj_grp_IO%grp_name, rj_grp_IO%item_grp)
!
      else
        rj_grp_IO%num_item = 0
        call allocate_grp_type_item(rj_grp_IO)
      end if
!
      end subroutine read_rj_grp_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_rj_grp_data_gz(rj_grp_IO)
!
      use m_sph_modes_grid_labels
      use t_group_data
!
      type(group_data), intent(inout) :: rj_grp_IO
!
!
      call write_group_data_gz(rj_grp_IO%num_grp,                       &
     &    rj_grp_IO%num_item, rj_grp_IO%istack_grp,                     &
     &    rj_grp_IO%grp_name, rj_grp_IO%item_grp)
!
      call deallocate_grp_type(rj_grp_IO)
!
      end subroutine write_rj_grp_data_gz
!
!------------------------------------------------------------------
!
      end module gz_sph_rj_groups_IO
