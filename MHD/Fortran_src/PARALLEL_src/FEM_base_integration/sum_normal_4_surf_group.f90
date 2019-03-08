!
!      module sum_normal_4_surf_group
!
!      Written by H. Matsui on Aug, 2006
!      Modified by H. Matsui on June, 2007
!      Modified by H. Matsui on Jan., 2009
!
!      subroutine s_sum_normal_4_surf_group(ele, sf_grp, sf_grp_v)
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
      subroutine s_sum_normal_4_surf_group(ele, sf_grp, sf_grp_v)
!
      use calypso_mpi
      use m_machine_parameter
      use t_geometry_data
      use t_group_data
      use t_surface_group_geometry
!
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_group_geometry), intent(inout) :: sf_grp_v
!
      integer(kind = kint) :: i
      integer(kind = kint_gl) :: num64
!
!
      call allocate_sum_local_area_grp(sf_grp%num_grp)
!
      call sum_norm_of_surf_group                                       &
     &   (np_smp, ele%numele, ele%interior_ele,                         &
     &    sf_grp%num_grp, sf_grp%num_item, sf_grp%item_sf_grp,          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    sf_grp_v%area_sf_grp)
!
      num64 = int(sf_grp%num_grp,KIND(num64))
      call calypso_mpi_allreduce_real                                   &
     &   (area_sf_grp_l, sf_grp_v%tot_area_sf_grp, num64, MPI_SUM)
      call deallocate_sum_local_area_grp
!
      if (my_rank.eq.0) then
        do i = 1, sf_grp%num_grp
           write(*,*) i, trim(sf_grp%grp_name(i)),                      &
     &                   sf_grp_v%tot_area_sf_grp(i)
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
      subroutine sum_norm_of_surf_group(np_smp, numele, interior_ele,   &
     &          num_surf, num_surf_bc, surf_item, num_surf_smp,         &
     &          isurf_grp_smp_stack, area_sf_grp)
!
      use calypso_mpi
!
      integer(kind = kint) , intent(in) :: numele
      integer (kind = kint), intent(in) :: interior_ele(numele)
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
            area_grp_smp(ip) = area_grp_smp(ip) + area_sf_grp(isurf)    &
     &                        * dble(interior_ele(iele))
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
      end subroutine sum_norm_of_surf_group
!
! ----------------------------------------------------------------------
!
      end module sum_normal_4_surf_group
