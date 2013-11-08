!
!      module cal_interpolate_coefs
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine allocate_work_4_interpolate
!      subroutine deallocate_work_4_interpolate
!
!      subroutine s_cal_interpolate_coefs(my_rank_org, inod, jele,      &
!     &          error_level, iflag_message)
!      subroutine check_interpolation(id_file, my_rank_org)
!
      module cal_interpolate_coefs
!
      use m_precision
!
      implicit none
!
      real(kind=kreal), allocatable :: coefs_by_tet(:)
      real(kind=kreal), allocatable :: x_local_ele(:,:)
      real(kind=kreal) :: differ_tmp, differ_res
!
      private :: coefs_by_tet, x_local_ele, differ_res
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_work_4_interpolate
!
      use m_2nd_geometry_param
!
      allocate( coefs_by_tet(nnod_4_ele_2nd) )
      allocate( x_local_ele(nnod_4_ele_2nd,3) )
!
      coefs_by_tet = 0.0d0
      x_local_ele = 0.0d0
!
      end subroutine allocate_work_4_interpolate
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_work_4_interpolate
!
      deallocate( coefs_by_tet )
      deallocate( x_local_ele )
!
      end subroutine deallocate_work_4_interpolate
!
! ----------------------------------------------------------------------
!
      subroutine s_cal_interpolate_coefs(my_rank_org, inod, jele,       &
     &          error_level, iflag_message, iflag_org_tmp)
!
      use m_ctl_params_4_gen_table
      use calypso_mpi
      use m_2nd_geometry_param
      use m_connect_hexa_2_tetra
      use subroutines_4_search_table
      use cal_local_position_by_tetra
      use modify_local_positions
      use solver_33_array
!
!
      integer (kind = kint), intent(in) :: my_rank_org, inod, jele
      integer (kind = kint), intent(in) :: iflag_message
      real (kind = kreal), intent(in) :: error_level
!
      integer (kind = kint), intent(inout) :: iflag_org_tmp
!
      real(kind=kreal) :: s_coef(3)
      real(kind=kreal) :: xi(3)
      real(kind=kreal) :: v_tetra(3,3)
      real(kind=kreal) :: x_target(3), v_target(3)
      real(kind=kreal) :: ref_error
!
      integer (kind = kint) :: itet, ierr_inter, i
!
!
      ierr_inter = 1
!
      call copy_target_local_vector(inod, x_target)
      call copy_position_2_2nd_local_ele(jele, x_local_ele)
!
      do itet = 1, num_tetra
        call cal_3vector_4_tet_2nd(itet, v_target, v_tetra,             &
     &      x_target, x_local_ele)
!
!   solve equations
!
        call solve_33_array(s_coef, v_target, v_tetra)
!
!   check solution
!
        call check_solution_in_tet(ref_error, s_coef)
!
!    satisfy the error level
!
        if ( abs(ref_error) .le. error_level) then
!
          call init_coefs_on_tet(itet, coefs_by_tet, s_coef)
!
          call s_cal_local_position_by_tetra(nnod_4_ele_2nd, xi,        &
     &       coefs_by_tet)
!
          if (iflag_message .eq. 1) then
            write(my_rank+60,*) inod, x_target(1:3)
            do i = 1, nnod_4_ele_2nd
              write(my_rank+60,*) i, jele, x_local_ele(i,1:3)
            end do
!            write(my_rank+60,*) 'coefs_by_tet', coefs_by_tet
            write(my_rank+60,*) 's_coef', s_coef
          end if
!
!     improve solution
!
          call s_modify_local_positions(maxitr, eps_iter, xi, x_target, &
     &        nnod_4_ele_2nd, x_local_ele, iflag_message, differ_res,   &
     &        ierr_inter)
!
!     finish improvement
!
          if (ierr_inter.gt.0 .and. ierr_inter.le.maxitr) then
             call set_results_2_array(my_rank_org, inod, jele, xi)
             go to 10
          else
            if (iflag_message .eq. 1) then
              write(my_rank+60,*)                                       &
     &                'improvement failed!!: inod, jele, itet:',        &
     &                 my_rank, inod, differ_tmp, differ_res,           &
     &                 my_rank_org, jele, itet, xi
              if (differ_res .lt. differ_tmp) then
                call set_results_2_array_fin(my_rank_org, inod, jele,   &
     &              xi, differ_tmp, differ_res, iflag_org_tmp)
              end if
            end if
          end if
!
        end if
      end do
!
  10  continue
!
      end subroutine s_cal_interpolate_coefs
!
!-----------------------------------------------------------------------
!
      subroutine check_interpolation(id_file, my_rank_org)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_geometry_param
      use m_interpolate_table_dest
      use m_interpolate_coefs_dest
      use m_work_const_itp_table
!
      use subroutines_4_search_table
      use cal_position_and_grad
!
      integer(kind = kint), intent(in) :: id_file, my_rank_org
!
      integer(kind = kint) :: inod
      real(kind=kreal) :: xx_z(3), xi(3), diff(3)
      real(kind=kreal) :: dnxi(3), dnei(3), dnzi(3)
!
      write(id_file,*) '#'
      write(id_file,*) '#  node, difference from original node'
      write(id_file,*) '#'
!
      do inod = 1, internal_node
!
        if( iflag_org_domain(inod) .eq. my_rank_org) then
!
          xi(1:3) = coef_inter_dest(inod,1:3)
          call copy_position_2_2nd_local_ele(iele_org_4_dest(inod),     &
     &        x_local_ele)
!
          call cal_position_and_gradient(nnod_4_ele_2nd, xx_z,          &
     &        dnxi, dnei, dnzi, x_local_ele, xi)
!
          diff(1:3) = xx_z(1:3) - xx(inod,1:3)
!
          write(id_file,*) inod, diff(1:3)
!
        end if
      end do
!
      end subroutine check_interpolation
!
!-----------------------------------------------------------------------
!
      end module cal_interpolate_coefs
