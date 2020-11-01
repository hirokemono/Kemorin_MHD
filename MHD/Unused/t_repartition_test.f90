!>@file  t_repartition_test.f90
!!       module t_repartition_test
!!
!!@author H. Matsui
!!@date   Programmed on Nov., 2008
!!@n      Modified by H. Matsui on Feb., 2012
!
!> @brief Neighbouring node and element list for each node
!!
!!@verbatim
!!      subroutine const_repartition_table                              &
!!     &         (ilevel_end, node, neib_nod)
!!        type(node_data), intent(in) :: node
!!        type(next_nod_id_4_nod), intent(in) :: neib_nod
!!@endverbatim
!
      module t_repartition_test
!
      use calypso_mpi
      use calypso_mpi_real
      use calypso_mpi_int
      use m_precision
      use m_constants
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_node_clustering_list
!
      implicit none
!
!
      type mul_node_clusterings
        integer(kind = kint) :: num_cluster
        type(node_clustering_list), allocatable :: cluster(:)
      end type mul_node_clusterings
!
      type repartition_list
        integer(kind = kint) :: num_list
        integer(kind = kint), allocatable :: inod_list(:)
      end type repartition_list
!
      integer(kind = kint), parameter :: max_grp = 100
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_repartition_table                                &
     &         (ilevel_end, mesh, neib_nod)
!
      use int_volume_of_single_domain
      use copy_mesh_structures
!
      integer(kind = kint), intent(in) :: ilevel_end
      type(mesh_geometry), intent(in) :: mesh
      type(next_nod_id_4_nod), intent(in) :: neib_nod
!
      type(node_clustering_list) :: first_ctr
      real(kind = kreal), allocatable :: volume_nod(:)
      integer(kind = kint), allocatable :: iflag_marked(:)
!
      real(kind = kreal), allocatable :: volume_nod_each(:)
      integer(kind = kint), allocatable :: istack_seed_each(:)
!
      real(kind = kreal) :: volume_nod_domain
      real(kind = kreal) :: volume_nod_total
      real(kind = kreal) :: volume_target
      real(kind = kreal) :: volume_count
      real(kind = kreal) :: vol_tmp, vol_rest
      integer(kind = kint) :: ist_new_pe, ied_new_pe, num_seed
      integer(kind = kint) :: num_seed_each
!
      integer(kind = kint), allocatable :: inod_seed(:)
      integer(kind = kint), allocatable :: istart_check(:)
      integer(kind = kint), allocatable :: iend_check(:)
      real(kind = kreal), allocatable :: volume_test(:)
!
      type(repartition_list), allocatable :: domain_lists(:)
!
      integer(kind = kint) :: ip, i, inod, icou
      integer(kind = kint) :: j, jnod, kcou, kst, ked, knum, knod
!
!
      write(*,*) 'estimate node volume'
      allocate(volume_nod(mesh%node%numnod))
      allocate(iflag_marked(mesh%node%numnod))
!
!$omp parallel workshare
      volume_nod(1:mesh%node%numnod) = 0.0d0
      iflag_marked(1:mesh%node%numnod) = 0
!$omp end parallel workshare
!
      call cal_node_volue(mesh%node, mesh%ele, volume_nod)
!
      volume_nod_domain = 0.0d0
!$omp parallel do reduction(+:volume_nod_domain)
      do i = 1, mesh%node%internal_node
        volume_nod_domain = volume_nod_domain + volume_nod(i)
      end do
!$omp end parallel do

      call calypso_mpi_allreduce_one_real                               &
     &   (volume_nod_domain, volume_nod_total, MPI_SUM)
      volume_target = volume_nod_total / dble(nprocs)
!
      write(*,*) my_rank, 'volume_nod_domain', volume_nod_domain
      if(my_rank.eq.0) then
        write(*,*) 'volume_nod_total', volume_nod_total
        write(*,*) 'volume_target', volume_target
      end if
!
      if(my_rank .eq. 0) then
        allocate(volume_nod_each(nprocs))
        allocate(istack_seed_each(0:nprocs))
      else
        allocate(volume_nod_each(0))
        allocate(istack_seed_each(0))
      end if
!
      call calypso_mpi_gather_one_real                                  &
     &   (volume_nod_domain, volume_nod_each, 0)
!
      if(my_rank .eq. 0) then
        vol_rest = 0.0d0
        do ip = 1, nprocs
          vol_tmp = volume_nod_each(ip) + vol_rest
          num_seed_each = vol_tmp / volume_target
          vol_rest = vol_tmp - dble(num_seed_each) * volume_target
          istack_seed_each(ip) = istack_seed_each(ip-1) + num_seed_each
          if(istack_seed_each(ip) .gt. nprocs) then
            istack_seed_each(ip) = nprocs
          end if
        end do
        istack_seed_each(nprocs) = nprocs
      end if
      if(my_rank.eq.0) write(*,*) 'istack_seed_each', istack_seed_each
      call calypso_mpi_barrier
      return
!
      call calypso_mpi_scatter_one_int                                  &
     &   (istack_seed_each(1), ist_new_pe, 0)
      call calypso_mpi_scatter_one_int                                  &
     &   (istack_seed_each(1), ied_new_pe, 0)
      num_seed = ied_new_pe - ist_new_pe
      volume_target = volume_nod_domain / num_seed
!
      allocate(domain_lists(nprocs))
      allocate(inod_seed(nprocs))
      allocate(istart_check(nprocs))
      allocate(iend_check(nprocs))
      allocate(volume_test(nprocs))
      istart_check(1:nprocs) = 0
      iend_check(1:nprocs) =   0
      volume_test(1:nprocs) = 0.0d0
!
      do ip = 1, nprocs
        domain_lists(ip)%num_list = 1
        allocate(domain_lists(ip)%inod_list(domain_lists(ip)%num_list))
      end do
!
      if(num_seed .gt. 0) then
        ip = ist_new_pe
        volume_count = volume_target / two
        do i = 1, mesh%node%internal_node
          volume_count = volume_count + volume_nod(i)
          if(volume_count .gt. volume_target) then
            volume_count = 0.0
            ip = ip + 1
            inod_seed(ip) = i
            istart_check(ip) = 1
            iend_check(ip) = 1
            if(icou .eq. ied_new_pe) exit
          end if
        end do
      end if
!
      do
        do ip = 1, nprocs
          if(istart_check(ip) .eq. 0) cycle
!
          do j = istart_check(ip), iend_check(ip)
            jnod = domain_lists(ip)%inod_list(j)
            kst = neib_nod%istack_next(jnod-1) + 1
            ked = neib_nod%istack_next(jnod)
            do knum = kst, ked
              knod = neib_nod%inod_next(knum)
              if(knod .gt. mesh%node%internal_node) cycle
              if(iflag_marked(knod) .ge. 0) cycle
!
              kcou = kcou + 1
!
              if(kcou .gt. domain_lists(ip)%num_list) then
                call extend_inod_list(domain_lists(ip))
              end if
!
              domain_lists(ip)%inod_list(kcou) = knod
              volume_test(ip) = volume_test(ip) + volume_nod(knod)
              iflag_marked(knod) = ip
              if(volume_test(ip) .gt. volume_count) go to 10
            end do
          end do
  10      continue
        end do
      end do
!
      deallocate(volume_nod)
!
      end subroutine const_repartition_table
!
!-----------------------------------------------------------------------
!
      subroutine extend_inod_list(domain_lists)
!
      type(repartition_list), intent(inout) :: domain_lists
      type(repartition_list) :: tmp_list
!
      tmp_list%num_list = domain_lists%num_list
      allocate(tmp_list%inod_list(tmp_list%num_list))
!
      tmp_list%inod_list(1:tmp_list%num_list)                           &
     &      = domain_lists%inod_list(1:tmp_list%num_list)
      deallocate(domain_lists%inod_list)
!
      domain_lists%num_list = 2 * domain_lists%num_list
      allocate(domain_lists%inod_list(domain_lists%num_list))
!
      domain_lists%inod_list(1:tmp_list%num_list)                       &
     &      = tmp_list%inod_list(1:tmp_list%num_list)
      deallocate(domain_lists%inod_list)
!
      end subroutine extend_inod_list
!
!-----------------------------------------------------------------------
!
      end module  t_repartition_test
