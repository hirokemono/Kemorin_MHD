!
!      module set_itp_destIO_2_org
!
!        programmed by H.Matsui on Sep. 2006
!
!      subroutine count_num_interpolation_4_orgin(n_org_rank,           &
!     &           n_dest_rank)
!      subroutine set_interpolation_4_orgin(n_org_rank, n_dest_rank)
!
      module set_itp_destIO_2_org
!
      use m_precision
!
      use m_interpolate_table_dest_IO
      use m_interpolate_table_orgin
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_num_interpolation_4_orgin(n_org_rank,            &
     &          n_dest_rank)
!
      integer(kind = kint), intent(in) :: n_org_rank, n_dest_rank
      integer(kind = kint) :: i
!
      do i = 1, num_org_domain_IO
!
        if (id_org_domain_IO(i) .eq. n_org_rank) then
          num_dest_domain = num_dest_domain + 1
          id_dest_domain(num_dest_domain) = n_dest_rank
          istack_nod_table_org(num_dest_domain)                         &
     &                       = istack_nod_table_org(num_dest_domain-1)  &
     &                        + istack_table_dest_IO(i)                 &
     &                        - istack_table_dest_IO(i-1)
        end if
      end do
!
      call deallocate_itp_nod_dst_IO
      call deallocate_itp_num_dst_IO
!
      end subroutine count_num_interpolation_4_orgin
!
!-----------------------------------------------------------------------
!
      subroutine set_interpolation_4_orgin(n_org_rank, n_dest_rank)
!
      integer(kind = kint), intent(in) :: n_org_rank, n_dest_rank
      integer(kind = kint) :: i, j, nnod, inum, iorg, idest
!
!
      do i = 1, num_org_domain_IO
        if (id_org_domain_IO(i) .eq. n_org_rank) then
          num_dest_domain = num_dest_domain + 1
!
          do j = 1, 4
            istack_nod_table_wtype_org(4*(num_dest_domain-1)+j)         &
     &         = istack_nod_table_wtype_org(4*(num_dest_domain-1)+j-1)  &
     &            + istack_table_wtype_dest_IO(4*(i-1)+j)               &
     &            - istack_table_wtype_dest_IO(4*(i-1)+j-1)
          end do
!
          nnod = istack_nod_table_org(num_dest_domain)                  &
     &          - istack_nod_table_org(num_dest_domain-1)
          do inum = 1, nnod
            iorg =  istack_nod_table_org(num_dest_domain-1) + inum
            idest = istack_table_dest_IO(i-1) + inum
!
            inod_gl_dest_4_org(iorg) = inod_global_dest_IO(idest)
            iele_org_4_org(iorg) =     iele_orgin_IO(idest)
            itype_inter_org(iorg) =    itype_inter_dest_IO(idest)
            coef_inter_org(iorg,1:3) = coef_inter_dest_IO(idest,1:3)
          end do
        end if
      end do
!
      call deallocate_itp_coefs_dst_IO
      call deallocate_itp_nod_dst_IO
      call deallocate_itp_num_dst_IO
!
      end subroutine set_interpolation_4_orgin
!
!-----------------------------------------------------------------------
!
      end module set_itp_destIO_2_org
