!gz_sph_rtp_groups_IO.f90
!      module gz_sph_rtp_groups_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_geom_rtp_groups_gz
!      subroutine write_geom_rtp_groups_gz
!
      module gz_sph_rtp_groups_IO
!
      use m_precision
!
      use m_constants
      use m_group_data_sph_specr_IO
      use gz_group_data_IO
      use skip_gz_comment
!
      implicit none
!
      private :: read_rtp_node_grp_data_gz
      private :: read_rtp_radial_grp_data_gz
      private :: read_rtp_theta_grp_data_gz
      private :: read_rtp_zonal_grp_data_gz
      private :: write_rtp_node_grp_data_gz
      private :: write_rtp_radial_grp_data_gz
      private :: write_rtp_theta_grp_data_gz
      private :: write_rtp_zonal_grp_data_gz
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtp_groups_gz
!
!
!      write(*,*) 'read_rtp_node_grp_data_gz'
      call read_rtp_node_grp_data_gz
!      write(*,*) 'read_rtp_radial_grp_data_gz'
      call read_rtp_radial_grp_data_gz
!      write(*,*) 'read_rtp_theta_grp_data_gz'
      call read_rtp_theta_grp_data_gz
!      write(*,*) 'read_rtp_zonal_grp_data_gz'
      call read_rtp_zonal_grp_data_gz
!
      end subroutine read_geom_rtp_groups_gz
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_groups_gz
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '! Group data', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_rtp_node_grp_data_gz
      call write_rtp_radial_grp_data_gz
      call write_rtp_theta_grp_data_gz
      call write_rtp_zonal_grp_data_gz
!
      end subroutine write_geom_rtp_groups_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_rtp_node_grp_data_gz
!
!
      call skip_gz_comment_int(num_bc_grp_rtp_IO)
      call allocate_rtp_nod_grp_IO_stack
!
      if (num_bc_grp_rtp_IO .gt. 0) then
        call read_group_stack_gz(num_bc_grp_rtp_IO,                     &
     &      ntot_bc_grp_rtp_IO, istack_bc_grp_rtp_IO)
!
        call allocate_rtp_nod_grp_IO_item
        call read_group_item_gz(num_bc_grp_rtp_IO,                      &
     &      ntot_bc_grp_rtp_IO, istack_bc_grp_rtp_IO,                   &
     &      name_bc_grp_rtp_IO,item_bc_grp_rtp_IO)
!
      else
        ntot_bc_grp_rtp_IO = 0
        call allocate_rtp_nod_grp_IO_item
      end if
!
      end subroutine read_rtp_node_grp_data_gz
!
!------------------------------------------------------------------
!
      subroutine read_rtp_radial_grp_data_gz
!
!
      call skip_gz_comment_int(num_radial_grp_rtp_IO)
      call allocate_rtp_r_grp_IO_stack
!
      if (num_radial_grp_rtp_IO .gt. 0) then
        call read_group_stack_gz(num_radial_grp_rtp_IO,                 &
     &      ntot_radial_grp_rtp_IO, istack_radial_grp_rtp_IO)
!
        call allocate_rtp_r_grp_IO_item
        call read_group_item_gz(num_radial_grp_rtp_IO,                  &
     &      ntot_radial_grp_rtp_IO, istack_radial_grp_rtp_IO,           &
     &      name_radial_grp_rtp_IO,item_radial_grp_rtp_IO)
!
      else
        ntot_radial_grp_rtp_IO = 0
        call allocate_rtp_r_grp_IO_item
      end if
!
      end subroutine read_rtp_radial_grp_data_gz
!
!------------------------------------------------------------------
!
      subroutine read_rtp_theta_grp_data_gz
!
!
      call skip_gz_comment_int(num_theta_grp_rtp_IO)
      call allocate_rtp_t_grp_IO_stack
!
      if (num_theta_grp_rtp_IO .gt. 0) then
        call read_group_stack_gz(num_theta_grp_rtp_IO,                  &
     &      ntot_theta_grp_rtp_IO, istack_theta_grp_rtp_IO)
!
        call allocate_rtp_t_grp_IO_item
        call read_group_item_gz(num_theta_grp_rtp_IO,                   &
     &      ntot_theta_grp_rtp_IO, istack_theta_grp_rtp_IO,             &
     &      name_theta_grp_rtp_IO,item_theta_grp_rtp_IO)
!
      else
        ntot_theta_grp_rtp_IO = 0
        call allocate_rtp_t_grp_IO_item
      end if
!
      end subroutine read_rtp_theta_grp_data_gz
!
!------------------------------------------------------------------
!
      subroutine read_rtp_zonal_grp_data_gz
!
!
      call skip_gz_comment_int(num_zonal_grp_rtp_IO)
      call allocate_rtp_p_grp_IO_stack
!
      if (num_zonal_grp_rtp_IO .gt. 0) then
        call read_group_stack_gz(num_zonal_grp_rtp_IO,                  &
     &      ntot_zonal_grp_rtp_IO, istack_zonal_grp_rtp_IO)
!
        call allocate_rtp_p_grp_IO_item
        call read_group_item_gz(num_zonal_grp_rtp_IO,                   &
     &      ntot_zonal_grp_rtp_IO, istack_zonal_grp_rtp_IO,             &
     &      name_zonal_grp_rtp_IO,item_zonal_grp_rtp_IO)
!
      else
        ntot_zonal_grp_rtp_IO = 0
        call allocate_rtp_p_grp_IO_item
      end if
!
      end subroutine read_rtp_zonal_grp_data_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_rtp_node_grp_data_gz
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '! Node groups', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
!
      call write_group_data_gz(num_bc_grp_rtp_IO, ntot_bc_grp_rtp_IO,   &
     &    istack_bc_grp_rtp_IO, name_bc_grp_rtp_IO, item_bc_grp_rtp_IO)
!
      call deallocate_rtp_nod_grp_IO_item
!
      end subroutine write_rtp_node_grp_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_rtp_radial_grp_data_gz
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '! radial groups', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
!
      call write_group_data_gz(num_radial_grp_rtp_IO,                   &
     &    ntot_radial_grp_rtp_IO, istack_radial_grp_rtp_IO,             &
     &    name_radial_grp_rtp_IO, item_radial_grp_rtp_IO)
!
      call deallocate_rtp_r_grp_IO_item
!
      end subroutine write_rtp_radial_grp_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_rtp_theta_grp_data_gz
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '! meridional groups', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
!
      call write_group_data_gz(num_theta_grp_rtp_IO,                    &
     &    ntot_theta_grp_rtp_IO, istack_theta_grp_rtp_IO,               &
     &    name_theta_grp_rtp_IO, item_theta_grp_rtp_IO)
!
      call deallocate_rtp_t_grp_IO_item
!
      end subroutine write_rtp_theta_grp_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_rtp_zonal_grp_data_gz
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '! zonal groups', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
!
      call write_group_data_gz(num_zonal_grp_rtp_IO,                    &
     &    ntot_zonal_grp_rtp_IO, istack_zonal_grp_rtp_IO,               &
     &    name_zonal_grp_rtp_IO, item_zonal_grp_rtp_IO)
!
      call deallocate_rtp_p_grp_IO_item
!
      end subroutine write_rtp_zonal_grp_data_gz
!
!------------------------------------------------------------------
!
      end module gz_sph_rtp_groups_IO
