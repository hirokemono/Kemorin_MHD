!merge_refine_itp_table.f90
!      module merge_refine_itp_table
!
!      subroutine set_merged_refine_data_org
!      subroutine sort_merge_itp_table_refine
!
      module merge_refine_itp_table
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
      use m_work_merge_refine_itp
!
      implicit none
!
      integer(kind = kint), private :: ie_new_five(48)
!
      real(kind = kreal), private :: xi_local_one(8,3)
      integer(kind = kint), parameter, private :: maxitr = 100
      real(kind = kreal), parameter, private :: eps_iter = 1.0e-11
!
      private :: copy_each_merge_itp_tbl_refine
      private :: set_5_refined_elements_local_posi
      private :: pick_local_positions_one_for_5
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_merged_refine_data_org
!
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_refine_flag_parameters
      use t_interpolate_tbl_org
      use copy_local_position_2_ele
      use modify_local_positions
!
      integer(kind = kint) :: iele_org, inod_org, k1
      integer(kind = kint) :: iele_1st
      integer(kind = kint) :: inod_2nd, ierr_modify
      integer(kind = kint) :: iref_flag_org, iref_org
      real(kind = kreal) :: xi_ele(3), xi_res(3), x_target(3)
      real(kind = kreal) :: x_local_ele(8,3)
      real(kind = kreal) :: differ_res
      integer(kind = kint), parameter :: istack_smp(0:1) = (/0, 1/)
!
!
      c2f_mgd%tbl_org%num_dest_domain                                   &
     &     = c2f_2nd%tbl_org%num_dest_domain
      c2f_mgd%tbl_org%iflag_self_itp_send                               &
     &     = c2f_2nd%tbl_org%iflag_self_itp_send
!
      c2f_mgd%tbl_org%ntot_table_org                                    &
     &     = c2f_2nd%tbl_org%ntot_table_org
!
      call alloc_type_itp_num_org(np_smp, c2f_mgd%tbl_org)
      call alloc_type_itp_table_org(c2f_mgd%tbl_org)
!
      c2f_mgd%tbl_org%id_dest_domain                                    &
     &     = c2f_2nd%tbl_org%id_dest_domain
      c2f_mgd%tbl_org%istack_nod_tbl_org                                &
     &     = c2f_2nd%tbl_org%istack_nod_tbl_org
      c2f_mgd%tbl_org%istack_itp_type_org                               &
     &     = c2f_2nd%tbl_org%istack_itp_type_org
!
!
      do inod_2nd = 1, c2f_mgd%tbl_org%ntot_table_org
        xi_ele(1:3) = c2f_2nd%tbl_org%coef_inter_org(inod_2nd,1:3)
        iele_1st = c2f_2nd%tbl_org%iele_org_4_org(inod_2nd)
!
        iref_flag_org = iflag_refine_ele_1st(iele_1st)
        iele_org = iele_org_1st(iele_1st,1)
        iref_org = iele_org_1st(iele_1st,2)
!
        c2f_mgd%tbl_org%inod_gl_dest_4_org(inod_2nd) = inod_2nd
        c2f_mgd%tbl_org%iele_org_4_org(inod_2nd) =     iele_org
!
        if(iref_flag_org  .eq. iflag_nothing) then
          c2f_mgd%tbl_org%inod_itp_send(inod_2nd) = inod_2nd
          c2f_mgd%tbl_org%coef_inter_org(inod_2nd,1:3)                  &
     &                = c2f_2nd%tbl_org%coef_inter_org(inod_2nd,1:3)
          c2f_mgd%tbl_org%itype_inter_org(inod_2nd)                     &
     &                = c2f_2nd%tbl_org%itype_inter_org(inod_2nd)
!
        else
          x_target(1:3) = xx_2nd(inod_2nd,1:3)
          do k1 = 1, nnod_4_ele
            inod_org = ie_org(iele_org,k1)
            x_local_ele(k1,1:3) = xx_org(inod_org,1:3)
          end do
!
          call set_5_refined_elements_local_posi(iref_flag_org)
          call pick_local_positions_one_for_5(iref_org)
!
          xi_ele(1:3) = c2f_2nd%tbl_org%coef_inter_org(inod_2nd,1:3)
          call set_local_position_by_itp(xi_local_one, xi_ele, xi_res)
!
          c2f_mgd%tbl_org%coef_inter_org(inod_2nd,1:3)  = xi_res(1:3)
!
!
          call s_modify_local_positions(maxitr, eps_iter, xi_res,       &
     &        x_target, ele_2nd%nnod_4_ele, x_local_ele, izero,         &
     &        differ_res, ierr_modify)
!
          call set_interpolate_flag_by_xi(xi_res,                       &
     &              c2f_mgd%tbl_org%itype_inter_org(inod_2nd))
          c2f_mgd%tbl_org%coef_inter_org(inod_2nd,1:3)  = xi_res(1:3)
!
          if(ierr_modify .gt. maxitr .or. ierr_modify.le.0) then
            write(56,*) 'modification failed!', inod_2nd,               &
     &                   c2f_mgd%tbl_org%itype_inter_org(inod_2nd),     &
     &                   ierr_modify, differ_res
          end if
!
        end if
!
      end do
!
      end subroutine  set_merged_refine_data_org
!
!   --------------------------------------------------------------------
!
      subroutine sort_merge_itp_table_refine
!
      use m_interpolate_table_orgin
!
      integer(kind = kint) :: icou, inod_2nd, iflag
!
!
      icou = 0
      do inod_2nd = 1, ntot_table_org
        iflag = c2f_mgd%tbl_org%itype_inter_org(inod_2nd)
        if( iflag .ge. 1 .and. iflag .le. 8) then
          icou = icou + 1
          call copy_each_merge_itp_tbl_refine(inod_2nd, icou)
        end if
      end do
      istack_itp_type_org(1) =  icou
!
      do inod_2nd = 1, ntot_table_org
        iflag = c2f_mgd%tbl_org%itype_inter_org(inod_2nd)
        if( iflag .ge. 101 .and. iflag .le. 112) then
          icou = icou + 1
          call copy_each_merge_itp_tbl_refine(inod_2nd, icou)
        end if
      end do
      istack_itp_type_org(2) =  icou
!
      do inod_2nd = 1, ntot_table_org
        iflag = c2f_mgd%tbl_org%itype_inter_org(inod_2nd)
        if( iflag .ge. 201 .and. iflag .le. 206) then
          icou = icou + 1
          call copy_each_merge_itp_tbl_refine(inod_2nd, icou)
        end if
      end do
      istack_itp_type_org(3) =  icou
!
      do inod_2nd = 1, ntot_table_org
        iflag = c2f_mgd%tbl_org%itype_inter_org(inod_2nd)
        if( iflag .eq. 0) then
          icou = icou + 1
          call copy_each_merge_itp_tbl_refine(inod_2nd, icou)
        end if
      end do
      istack_itp_type_org(4) =  icou
      istack_nod_tbl_org(1) =   istack_itp_type_org(4)
!
!
      end subroutine sort_merge_itp_table_refine
!
! ----------------------------------------------------------------------
!
      subroutine copy_each_merge_itp_tbl_refine(inod_2nd, icou)
!
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
!
      integer(kind = kint), intent(in) :: inod_2nd, icou
!
!
          c2f_mgd%tbl_org%inod_itp_send(inod_2nd) = inod_2nd
          inod_gl_dest_4_org(icou)                                      &
     &         = c2f_mgd%tbl_org%inod_gl_dest_4_org(inod_2nd)
          iele_org_4_org(icou)                                          &
     &         = c2f_mgd%tbl_org%iele_org_4_org(inod_2nd)
          itype_inter_org(icou)                                         &
     &         = c2f_mgd%tbl_org%itype_inter_org(inod_2nd)
          coef_inter_org(icou,1:3)                                      &
     &         = c2f_mgd%tbl_org%coef_inter_org(inod_2nd,1:3)
!
          inod_dest_4_dest(icou) = inod_2nd
!
      end subroutine copy_each_merge_itp_tbl_refine
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_5_refined_elements_local_posi(iflag_refine)
!
      use m_refine_flag_parameters
      use m_refined_connection_tbl
      use m_work_merge_refine_itp
!
      integer(kind = kint), intent(in) :: iflag_refine
      integer(kind = kint) :: nnod_5
!
!
      if (iflag_refine .eq. iflag_nothing) then
        ie_new_five(1) = 1
        ie_new_five(2) = 4
        ie_new_five(3) = 16
        ie_new_five(4) = 13
        ie_new_five(5) = 49
        ie_new_five(6) = 52
        ie_new_five(7) = 64
        ie_new_five(8) = 61
      else if(iflag_refine .eq. iflag_five_x) then
        nnod_5 = 40
        ie_new_five(1:nnod_5) = ie_new_five_wx(1:nnod_5)
      else if(iflag_refine .eq. iflag_five_y) then
        nnod_5 = 40
        ie_new_five(1:nnod_5) = ie_new_five_wy(1:nnod_5)
      else if(iflag_refine .eq. iflag_five_z) then
        nnod_5 = 40
        ie_new_five(1:nnod_5) = ie_new_five_wz(1:nnod_5)
!
      else if(iflag_refine .eq. iflag_five_s1) then
        nnod_5 = 48
        ie_new_five(1:nnod_5) = ie_new_five_s1(1:nnod_5)
      else if(iflag_refine .eq. iflag_five_s2) then
        nnod_5 = 48
        ie_new_five(1:nnod_5) = ie_new_five_s2(1:nnod_5)
      else if(iflag_refine .eq. iflag_five_s3) then
        nnod_5 = 48
        ie_new_five(1:nnod_5) = ie_new_five_s3(1:nnod_5)
      else if(iflag_refine .eq. iflag_five_s4) then
        nnod_5 = 48
        ie_new_five(1:nnod_5) = ie_new_five_s4(1:nnod_5)
      else if(iflag_refine .eq. iflag_five_s5) then
        nnod_5 = 48
        ie_new_five(1:nnod_5) = ie_new_five_s5(1:nnod_5)
      else if(iflag_refine .eq. iflag_five_s6) then
        nnod_5 = 48
        ie_new_five(1:nnod_5) = ie_new_five_s6(1:nnod_5)
      end if
!
      end subroutine set_5_refined_elements_local_posi
!
! ----------------------------------------------------------------------
!
      subroutine pick_local_positions_one_for_5(i_refine)
!
      integer(kind = kint), intent(in) :: i_refine
!
      integer(kind = kint) :: i, j, inod
!
!
      do i = 1, 8
        j = i + (i_refine-1) * 8
        inod = ie_new_five(j)
        xi_local_one(i,1) = xi_refine_local_tri(1,inod)
        xi_local_one(i,2) = xi_refine_local_tri(2,inod)
        xi_local_one(i,3) = xi_refine_local_tri(3,inod)
      end do
!
      end subroutine pick_local_positions_one_for_5
!
! ----------------------------------------------------------------------
!
      subroutine set_local_position_by_itp(xi_ele, xi_itp, vect)
!
      use shape_func_elements
      use shape_func_3d_linear
      use interporate_position_in_ele
!
      real(kind = kreal), intent(in) :: xi_ele(8,3)
      real(kind = kreal), intent(in) :: xi_itp(3)
      real(kind = kreal), intent(inout) :: vect(3)
!
      real (kind=kreal) :: xi_nega, ei_nega, zi_nega
      real (kind=kreal) :: xi_posi, ei_posi, zi_posi
      real (kind=kreal) :: xi_sqre, ei_sqre, zi_sqre
      real (kind=kreal) :: an(8)
!
!
      call s_shape_elenents_aw_3d(xi_nega, ei_nega, zi_nega,            &
     &    xi_posi, ei_posi, zi_posi, xi_sqre, ei_sqre, zi_sqre,         &
     &    xi_itp(1), xi_itp(2), xi_itp(3) )
!
      call shape_function_an_1(an, xi_nega, ei_nega, zi_nega,           &
     &      xi_posi, ei_posi, zi_posi)
      call interporate_one_position_linear(vect, xi_ele, an)
!
      end subroutine set_local_position_by_itp
!
! ----------------------------------------------------------------------
!
      end module merge_refine_itp_table
