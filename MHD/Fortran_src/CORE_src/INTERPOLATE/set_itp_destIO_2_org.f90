!
!      module set_itp_destIO_2_org
!
!        programmed by H.Matsui on Sep. 2006
!
!!      subroutine count_num_interpolation_4_orgin                      &
!!     &         (n_org_rank, n_dest_rank, IO_dest, itp_org)
!!      subroutine set_interpolation_4_orgin                            &
!!     &          (n_org_rank, IO_dest, IO_coef_dest, itp_org)
!!        type(interpolate_table_dest), intent(inout) :: IO_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_coef_dest
!!        type(interpolate_table_org), intent(inout) :: itp_org
!
      module set_itp_destIO_2_org
!
      use m_precision
!
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_num_interpolation_4_orgin                        &
     &         (n_org_rank, n_dest_rank, IO_dest, itp_org)
!
      integer(kind = kint), intent(in) :: n_org_rank, n_dest_rank
      type(interpolate_table_dest), intent(inout) :: IO_dest
      type(interpolate_table_org), intent(inout) :: itp_org
!
      integer(kind = kint) :: i
!
      do i = 1, IO_dest%num_org_domain
!
        if (IO_dest%id_org_domain(i) .eq. n_org_rank) then
          itp_org%num_dest_domain = itp_org%num_dest_domain + 1
          itp_org%id_dest_domain(itp_org%num_dest_domain)               &
     &       = n_dest_rank
          itp_org%istack_nod_tbl_org(itp_org%num_dest_domain)           &
     &       = itp_org%istack_nod_tbl_org(itp_org%num_dest_domain-1)    &
     &                        + IO_dest%istack_nod_tbl_dest(i)          &
     &                        - IO_dest%istack_nod_tbl_dest(i-1)
        end if
      end do
!
      call dealloc_itp_table_dest(IO_dest)
      call dealloc_itp_num_dest(IO_dest)
!
      end subroutine count_num_interpolation_4_orgin
!
!-----------------------------------------------------------------------
!
      subroutine set_interpolation_4_orgin                              &
     &          (n_org_rank, IO_dest, IO_coef_dest, itp_org)
!
      use m_work_const_itp_table
!
      integer(kind = kint), intent(in) :: n_org_rank
      type(interpolate_table_dest), intent(inout) :: IO_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_coef_dest
!
      type(interpolate_table_org), intent(inout) :: itp_org
!
      integer(kind = kint) :: i, j, nnod, inum, iorg, idest
!
!
      do i = 1, IO_dest%num_org_domain
        if (IO_dest%id_org_domain(i) .eq. n_org_rank) then
          itp_org%num_dest_domain = itp_org%num_dest_domain + 1
!
          do j = 1, 4
            istack_org_para_type(4*(itp_org%num_dest_domain-1)+j)       &
     &       = istack_org_para_type(4*(itp_org%num_dest_domain-1)+j-1)  &
     &        + IO_coef_dest%istack_nod_tbl_wtype_dest(4*(i-1)+j)       &
     &        - IO_coef_dest%istack_nod_tbl_wtype_dest(4*(i-1)+j-1)
          end do
!
          nnod = itp_org%istack_nod_tbl_org(itp_org%num_dest_domain)    &
     &        - itp_org%istack_nod_tbl_org(itp_org%num_dest_domain-1)
          do inum = 1, nnod
            iorg                                                        &
     &       =  itp_org%istack_nod_tbl_org(itp_org%num_dest_domain-1)   &
     &        + inum
            idest = IO_dest%istack_nod_tbl_dest(i-1) + inum
!
            itp_org%inod_itp_send(iorg) =      iorg
            itp_org%inod_gl_dest_4_org(iorg)                            &
     &            = IO_coef_dest%inod_gl_dest(idest)
            itp_org%iele_org_4_org(iorg)                                &
     &            = IO_coef_dest%iele_org_4_dest(idest)
            itp_org%itype_inter_org(iorg)                               &
     &            = IO_coef_dest%itype_inter_dest(idest)
            itp_org%coef_inter_org(iorg,1:3)                            &
     &            = IO_coef_dest%coef_inter_dest(idest,1:3)
          end do
        end if
      end do
!
      call dealloc_itp_coef_dest(IO_coef_dest)
      call dealloc_itp_coef_stack(IO_coef_dest)
      call dealloc_itp_num_dest(IO_dest)
      call dealloc_itp_table_dest(IO_dest)
!
      end subroutine set_interpolation_4_orgin
!
!-----------------------------------------------------------------------
!
      end module set_itp_destIO_2_org
