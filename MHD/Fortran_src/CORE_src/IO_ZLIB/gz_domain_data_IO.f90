!
!      module gz_domain_data_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_domain_info_gz
!      subroutine read_import_data_gz
!      subroutine read_export_data_gz
!      subroutine write_domain_info_gz
!      subroutine write_import_data_gz
!      subroutine write_export_data_gz
!
      module gz_domain_data_IO
!
      use m_precision
!
      use m_comm_data_IO
      use skip_gz_comment
!
      implicit none
!
      private :: write_send_recv_data_gz, read_send_recv_item_gz
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine read_domain_info_gz
!
!
      call skip_gz_comment_int(my_rank_IO)
!
      call get_one_line_from_gz_f
      read(textbuf,*) comm_IO%num_neib
!
      call allocate_neib_domain_IO
!
      if (comm_IO%num_neib .gt. 0) then
        call read_gz_multi_int(comm_IO%num_neib, comm_IO%id_neib)
      end if
!
      end subroutine read_domain_info_gz
!
!------------------------------------------------------------------
!
      subroutine read_import_data_gz
!
!
      call allocate_import_stack_IO
!
      comm_IO%istack_import(0) = 0
      if (comm_IO%num_neib .gt. 0) then
        call read_gz_integer_stack(comm_IO%num_neib,                    &
     &      comm_IO%istack_import, comm_IO%ntot_import)
!
        call allocate_import_item_IO
        call read_send_recv_item_gz                                     &
     &     (comm_IO%ntot_import, comm_IO%item_import)
      else
        comm_IO%ntot_import = 0
        call allocate_import_item_IO
      end if
!
      end subroutine read_import_data_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_export_data_gz
!
!
      call allocate_export_stack_IO
!
      comm_IO%istack_export(0) = 0
      if (comm_IO%num_neib .gt. 0) then
        call read_gz_integer_stack(comm_IO%num_neib,                    &
     &      comm_IO%istack_export, comm_IO%ntot_export)
!
        call allocate_export_item_IO
        call read_send_recv_item_gz                                     &
     &     (comm_IO%ntot_export, comm_IO%item_export)
      else
        comm_IO%ntot_export = 0
        call allocate_export_item_IO
      end if
!
      end subroutine read_export_data_gz
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_domain_info_gz
!
!
      write(textbuf,'(i16,a1)') my_rank_IO, char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(i16,a1)') comm_IO%num_neib, char(0)
      call gz_write_textbuf_w_lf
!
      if (comm_IO%num_neib .gt. 0) then
        call write_gz_multi_int_8i10(comm_IO%num_neib,                  &
     &      comm_IO%id_neib)
      else
        write(textbuf,'(a1)') char(0)
        call gz_write_textbuf_w_lf
      end if
!
      call deallocate_neib_domain_IO
!
      end subroutine write_domain_info_gz
!
!------------------------------------------------------------------
!
      subroutine write_import_data_gz
!
      call write_send_recv_data_gz                                      &
     &   (comm_IO%num_neib, comm_IO%ntot_import,                        &
     &    comm_IO%istack_import, comm_IO%item_import)
!
      call deallocate_import_item_IO
!
      end subroutine write_import_data_gz
!
! -----------------------------------------------------------------------
!
      subroutine write_export_data_gz
!
!
      call write_send_recv_data_gz                                      &
     &   (comm_IO%num_neib, comm_IO%ntot_export,                        &
     &    comm_IO%istack_export, comm_IO%item_export)
!
      call deallocate_export_item_IO
!
      end subroutine write_export_data_gz
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_send_recv_item_gz(ntot_sr, inod_sr)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: ntot_sr
      integer(kind = kint), intent(inout) :: inod_sr(ntot_sr)
!
      integer(kind = kint) :: i
!
      call skip_gz_comment_int( inod_sr(1) )
      do i = 2, ntot_sr
        call get_one_line_from_gz_f
        read(textbuf,*) inod_sr(i)
      end do
!
      end subroutine read_send_recv_item_gz
!
! -----------------------------------------------------------------------
!
      subroutine write_send_recv_data_gz(num_sr, ntot_sr, istack_sr,    &
     &          inod_sr)
!
      integer(kind = kint), intent(in) :: num_sr, ntot_sr
      integer(kind = kint), intent(in) :: istack_sr(0:num_sr)
      integer(kind = kint), intent(in) :: inod_sr(ntot_sr)
!
      integer(kind = kint) :: i
!
      if (num_sr .gt. 0) then
        call write_gz_multi_int_8i10(num_sr, istack_sr(1))
        do i = 1, ntot_sr
          write(textbuf,'(i16,a1)') inod_sr(i), char(0)
          call gz_write_textbuf_w_lf
        end do
      else
        write(textbuf,'(a1)') char(0)
        call gz_write_textbuf_w_lf
      end if
!
      end subroutine write_send_recv_data_gz
!
! -----------------------------------------------------------------------
!
      end module gz_domain_data_IO
