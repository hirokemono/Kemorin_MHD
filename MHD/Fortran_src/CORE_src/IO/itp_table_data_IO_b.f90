!itp_table_data_IO_b.f90
!      module itp_table_data_IO_b
!
!        programmed by H.Matsui on Sep. 2006 (ver 1.2)
!
!      subroutine write_interpolate_domain_org_b(id_file, my_rank)
!      subroutine write_interpolate_table_org_b(id_file)
!
!      subroutine read_interpolate_domain_org_b(id_file, n_rank)
!      subroutine read_interpolate_table_org_b(id_file)
!
!      subroutine write_interpolate_table_dest_b(id_file, my_rank)
!      subroutine write_interpolate_coefs_dest_b(id_file)
!
!      subroutine read_interpolate_domain_dest_b(id_file, n_rank)
!      subroutine read_interpolate_table_dest_b(id_file)
!      subroutine read_interpolate_coefs_dest_b(id_file)
!
      module itp_table_data_IO_b
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_domain_org_b(id_file, my_rank)
!
      use m_interpolate_table_org_IO
!
      integer(kind = kint), intent(in) :: id_file, my_rank
!
!
      write(id_file) my_rank
      write(id_file) num_dest_domain_IO
!
      if (num_dest_domain_IO .gt. 0) then
        write(id_file) id_dest_domain_IO(1:num_dest_domain_IO)
      end if
!
!
      end subroutine write_interpolate_domain_org_b
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_table_org_b(id_file)
!
      use m_interpolate_table_org_IO
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: i
!
      if (num_dest_domain_IO .gt. 0) then
        write(id_file) istack_table_wtype_org_IO(0:num_dest_domain_IO)
!
        write(id_file) inod_gl_dest_4_org_IO(1:ntot_table_org_IO)
        write(id_file) iele_org_4_org_IO(1:ntot_table_org_IO)
        write(id_file) inod_gl_dest_4_org_IO(1:ntot_table_org_IO)
        do i = 1, 3
          write(id_file) coef_inter_org_IO(1:ntot_table_org_IO,i)
        end do
!
      end if
!
      end subroutine write_interpolate_table_org_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_domain_org_b(id_file, n_rank)
!
      use m_interpolate_table_org_IO
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(inout) :: n_rank
!
!
      read(id_file) n_rank
      read(id_file) num_dest_domain_IO
!
      if (num_dest_domain_IO .gt. 0) then
!
        call allocate_itp_num_org_IO
!
        read(id_file) id_dest_domain_IO(1:num_dest_domain_IO)
!
      end if
!
      end subroutine read_interpolate_domain_org_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_table_org_b(id_file)
!
      use m_interpolate_table_org_IO
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: i
!
!
      if (num_dest_domain_IO .gt. 0) then
        read(id_file) istack_table_wtype_org_IO(0:num_dest_domain_IO)
!
        ntot_table_org_IO                                               &
     &        = istack_table_wtype_org_IO(4*num_dest_domain_IO)
!
        call allocate_itp_table_org_IO
!
        read(id_file) inod_gl_dest_4_org_IO(1:ntot_table_org_IO)
        read(id_file) iele_org_4_org_IO(1:ntot_table_org_IO)
        read(id_file) inod_gl_dest_4_org_IO(1:ntot_table_org_IO)
        do i = 1, 3
          read(id_file) coef_inter_org_IO(1:ntot_table_org_IO,i)
        end do
!
      end if
!
      end subroutine read_interpolate_table_org_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_table_dest_b(id_file, my_rank)
!
      use m_interpolate_table_dest_IO
!
      integer(kind = kint), intent(in) :: id_file, my_rank
!
!
      write(id_file) my_rank
      write(id_file) num_org_domain_IO
!
      if (num_org_domain_IO .gt. 0) then
        write(id_file) id_org_domain_IO(1:num_org_domain_IO)
!
        write(id_file) istack_table_dest_IO(1:num_org_domain_IO)
        write(id_file) inod_dest_IO(1:ntot_table_dest_IO)
!
      end if
!
      end subroutine write_interpolate_table_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_coefs_dest_b(id_file)
!
      use m_interpolate_table_dest_IO
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: i
!
      if (num_org_domain_IO .gt. 0) then
!
        write(id_file)                                                  &
     &          istack_table_wtype_dest_IO(0:4*num_org_domain_IO)
!
        write(id_file) inod_global_dest_IO(1:ntot_table_dest_IO)
        write(id_file) iele_orgin_IO(1:ntot_table_dest_IO)
        write(id_file) itype_inter_dest_IO(1:ntot_table_dest_IO)
        do i = 1, 3
          write(id_file) coef_inter_dest_IO(1:ntot_table_dest_IO,i)
        end do
!
      end if
!
      end subroutine write_interpolate_coefs_dest_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_domain_dest_b(id_file, n_rank)
!
      use m_interpolate_table_dest_IO
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(inout) :: n_rank
!
!
      read(id_file) n_rank
      read(id_file) num_org_domain_IO
!
      if (num_org_domain_IO .gt. 0) then
        call allocate_itp_num_dst_IO
        read(id_file) id_org_domain_IO(1:num_org_domain_IO)
      end if
!
      end subroutine read_interpolate_domain_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_table_dest_b(id_file)
!
      use m_interpolate_table_dest_IO
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
!
!
      if (num_org_domain_IO .gt. 0) then
!
        read(id_file) istack_table_dest_IO(1:num_org_domain_IO)
        ntot_table_dest_IO = istack_table_dest_IO(num_org_domain_IO)
!
        call allocate_itp_nod_dst_IO
        read(id_file) inod_dest_IO(1:ntot_table_dest_IO)
!
      end if
!
      end subroutine read_interpolate_table_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_coefs_dest_b(id_file)
!
      use m_interpolate_table_dest_IO
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: i
!
!
      if (num_org_domain_IO .gt. 0) then
!
        read(id_file) istack_table_wtype_dest_IO(0:num_org_domain_IO)
        ntot_table_dest_IO                                              &
     &        = istack_table_wtype_dest_IO(4*num_org_domain_IO)
!
        call allocate_itp_coefs_dst_IO
!
        read(id_file) inod_global_dest_IO(1:ntot_table_dest_IO)
        read(id_file) iele_orgin_IO(1:ntot_table_dest_IO)
        read(id_file) itype_inter_dest_IO(1:ntot_table_dest_IO)
        do i = 1, 3
          read(id_file) coef_inter_dest_IO(1:ntot_table_dest_IO,i)
        end do
      end if
!
      end subroutine read_interpolate_coefs_dest_b
!
!-----------------------------------------------------------------------
!
      end module itp_table_data_IO_b
