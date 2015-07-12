!
!      module sum_normal_4_surf_group
!
!      Written by H. Matsui on Aug, 2006
!      Modified by H. Matsui on June, 2007
!      Modified by H. Matsui on Jan., 2009
!
!      subroutine s_sum_normal_4_surf_group
!      subroutine s_sum_norm_of_surf_grp_para(num_surf, tot_area_sf_grp)
!
      module sum_normal_4_surf_group
!
      use m_precision
!
      implicit none
!
      real(kind= kreal), allocatable :: area_sf_grp_l(:)
      real(kind= kreal), allocatable :: area_grp_smp(:)
!
      private :: area_sf_grp_l, area_grp_smp
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_sum_normal_4_surf_group
!
      use calypso_mpi
      use m_geometry_parameter
      use m_machine_parameter
      use m_geometry_data
      use m_surface_group
      use m_surface_group_geometry
!
      integer(kind = kint) :: i
!
!
      call allocate_sum_local_area_grp(sf_grp1%num_grp)
!
      call s_sum_norm_of_surf_group(np_smp, numele, e_multi,            &
     &    sf_grp1%num_grp, sf_grp1%num_item, sf_grp1%item_sf_grp,       &
     &    sf_grp1%num_grp_smp, sf_grp1%istack_grp_smp,                  &
     &    sf_grp_v1%area_sf_grp)
!
      call s_sum_norm_of_surf_grp_para                                  &
     &   (sf_grp1%num_grp, sf_grp_v1%tot_area_sf_grp)
      call deallocate_sum_local_area_grp
!
      if (my_rank.eq.0) then
        do i = 1, sf_grp1%num_grp
           write(*,*) i, trim(sf_grp1%grp_name(i)),                     &
     &                   sf_grp_v1%tot_area_sf_grp(i)
        end do
      end if 
!
      end subroutine s_sum_normal_4_surf_group
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_sum_local_area_grp(n_surf_grp)
!
      use m_machine_parameter
      integer(kind = kint), intent(in) :: n_surf_grp
!
      allocate(area_sf_grp_l(n_surf_grp))
      allocate(area_grp_smp(np_smp))
!
      area_sf_grp_l = 0.0d0
      area_grp_smp = 0.0d0
!
      end subroutine allocate_sum_local_area_grp
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_sum_local_area_grp
!
      deallocate(area_sf_grp_l)
      deallocate(area_grp_smp)
!
      end subroutine deallocate_sum_local_area_grp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_sum_norm_of_surf_group(np_smp, numele, e_multi,      &
     &          num_surf, num_surf_bc, surf_item, num_surf_smp,         &
     &          isurf_grp_smp_stack, area_sf_grp)
!
      use calypso_mpi
!
      integer(kind = kint) , intent(in) :: numele
      real(kind = kreal), intent(in) :: e_multi(numele)
      integer(kind = kint), intent(in) :: np_smp, num_surf_smp
      integer(kind = kint), intent(in)                                  &
     &            :: isurf_grp_smp_stack(0:num_surf_smp)
      integer(kind = kint), intent(in) :: num_surf, num_surf_bc
      integer(kind = kint), intent(in) :: surf_item(2,num_surf_bc)
      real(kind = kreal), intent(in) :: area_sf_grp(num_surf_bc)
!
      integer(kind = kint) :: i_grp, ip, i, ist, ied, isurf, iele
!
!
      area_sf_grp_l = 0.0d0
!
      do i_grp = 1, num_surf
!
        area_grp_smp = 0.0d0
!
!$omp parallel do private(ist,ied,isurf,iele)
        do ip = 1, np_smp
          i = (i_grp-1)*np_smp + ip
          ist = isurf_grp_smp_stack(i-1) + 1
          ied = isurf_grp_smp_stack(i)
!
          do isurf = ist, ied
            iele = surf_item(1,isurf)
            area_grp_smp(ip) = area_grp_smp(ip)                         &
     &                        + area_sf_grp(isurf) * e_multi(iele)
          end do
        end do
!$omp end parallel do
!
        do ip = 1, np_smp
          area_sf_grp_l(i_grp) = area_sf_grp_l(i_grp)                   &
     &                          + area_grp_smp(ip)
        end do
      end do
!
      end subroutine s_sum_norm_of_surf_group
!
! ----------------------------------------------------------------------
!
      subroutine s_sum_norm_of_surf_grp_para(num_surf, tot_area_sf_grp)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: num_surf
      real(kind = kreal), intent(inout) :: tot_area_sf_grp(num_surf)
!
!
      call MPI_allREDUCE (area_sf_grp_l, tot_area_sf_grp, num_surf,     &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine s_sum_norm_of_surf_grp_para
!
! ----------------------------------------------------------------------
!
      end module sum_normal_4_surf_group
