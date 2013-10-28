!
!      module copy_interpolate_org_IO
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine copy_itp_table_org_from_IO(my_rank)
!      subroutine copy_itp_table_org_to_IO
!
      module copy_interpolate_org_IO
!
      use m_precision
!
      use m_interpolate_table_orgin
      use m_interpolate_table_org_IO
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_itp_table_org_from_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      iflag_self_itp_send = 0
      num_dest_domain = num_dest_domain_IO
!
      if (num_dest_domain .gt. 0) then
!
        ntot_table_org = ntot_table_org_IO
!
        call allocate_itp_num_org(num_dest_domain)
        call allocate_itp_table_org
!
        id_dest_domain(1:num_dest_domain)                               &
     &          = id_dest_domain_IO(1:num_dest_domain)
        istack_nod_tbl_org(0:num_dest_domain)                           &
     &          = istack_nod_table_org_IO(0:num_dest_domain)
        istack_nod_tbl_wtype_org(0:4*num_dest_domain)                   &
     &          = istack_table_wtype_org_IO(0:4*num_dest_domain)
!
        inod_itp_send(1:ntot_table_org)                                 &
     &          = inod_itp_send_IO(1:ntot_table_org)
        inod_gl_dest_4_org(1:ntot_table_org)                            &
     &          = inod_gl_dest_4_org_IO(1:ntot_table_org)
        iele_org_4_org(1:ntot_table_org)                                &
     &          = iele_org_4_org_IO(1:ntot_table_org)
        itype_inter_org(1:ntot_table_org)                               &
     &          = itype_inter_org_IO(1:ntot_table_org)
!
        coef_inter_org(1:ntot_table_org,1)                              &
     &          = coef_inter_org_IO(1:ntot_table_org,1)
        coef_inter_org(1:ntot_table_org,2)                              &
     &          = coef_inter_org_IO(1:ntot_table_org,2)
        coef_inter_org(1:ntot_table_org,3)                              &
     &          = coef_inter_org_IO(1:ntot_table_org,3)
!
        call deallocate_itp_table_org_IO
        call deallocate_itp_num_org_IO
!
        if ( id_dest_domain(num_dest_domain) .eq. my_rank) then
          iflag_self_itp_send = 1
        end if
!
      end if
!
      end subroutine copy_itp_table_org_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine copy_itp_table_org_to_IO
!
!
       num_dest_domain_IO = num_dest_domain
!
      if (num_dest_domain .gt. 0) then
!
        ntot_table_org_IO = ntot_table_org
!
        call allocate_itp_num_org_IO
        call allocate_itp_table_org_IO
!
        id_dest_domain_IO(1:num_dest_domain)                            &
     &          = id_dest_domain(1:num_dest_domain)
        istack_nod_table_org_IO(0:num_dest_domain)                      &
     &          = istack_nod_tbl_org(0:num_dest_domain)
        istack_table_wtype_org_IO(0:4*num_dest_domain)                  &
     &          = istack_nod_tbl_wtype_org(0:4*num_dest_domain)
!
!
        inod_itp_send_IO(1:ntot_table_org)                              &
     &          = inod_itp_send(1:ntot_table_org)
        inod_gl_dest_4_org_IO(1:ntot_table_org)                         &
     &          = inod_gl_dest_4_org(1:ntot_table_org)
        iele_org_4_org_IO(1:ntot_table_org)                             &
     &          = iele_org_4_org(1:ntot_table_org)
        itype_inter_org_IO(1:ntot_table_org)                            &
     &          = itype_inter_org(1:ntot_table_org)
!
        coef_inter_org_IO(1:ntot_table_org,1)                           &
     &          = coef_inter_org(1:ntot_table_org,1)
        coef_inter_org_IO(1:ntot_table_org,2)                           &
     &          = coef_inter_org(1:ntot_table_org,2)
        coef_inter_org_IO(1:ntot_table_org,3)                           &
     &          = coef_inter_org(1:ntot_table_org,3)
!
      end if
!
      call deallocate_itp_table_org
      call deallocate_itp_num_org
!
      end subroutine copy_itp_table_org_to_IO
!
!-----------------------------------------------------------------------
!
      end module copy_interpolate_org_IO
