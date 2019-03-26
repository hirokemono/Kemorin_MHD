!set_filters_4_new_domains.f90
!     module set_filters_4_new_domains
!
!     Writteg by H.Matsui on Apr., 2008
!
!!      subroutine allocate_imark_whole_nod(nnod_global)
!!      subroutine deallocate_imark_whole_nod
!!      subroutine clear_imark_whole_nod
!!
!!      subroutine nod_marking_by_filtering_data                        &
!!     &         (numnod, internal_node, inod_global, xx,               &
!!     &          ip2, nod_d_grp, whole_fil_sort, fluid_fil_sort)
!!        type(domain_group_4_partition), intent(in) :: nod_d_grp
!!        type(filter_func_4_sorting), intent(in) :: whole_fil_sort
!!        type(filter_func_4_sorting), intent(in) :: fluid_fil_sort
!!      subroutine set_global_nodid_4_newfilter(nod_d_grp)
!!        type(domain_group_4_partition), intent(inout) :: nod_d_grp
!!
!!      subroutine set_num_globalnod_4_newdomain                        &
!!     &         (ip2, nod_d_grp, itl_nod_part, new_node)
!!        type(domain_group_4_partition), intent(in) :: nod_d_grp
!!        type(internal_4_partitioner), intent(inout) :: itl_nod_part
!!        type(node_data), intent(inout) :: new_node
!!      subroutine set_newdomain_filtering_nod(ip2, itl_nod_part)
!!        type(internal_4_partitioner), intent(in) :: itl_nod_part
!!
!!      subroutine set_filter_for_new_each_domain                       &
!!     &         (numnod, internal_node, inod_global, ip2, inter_nod_f, &
!!     &          nod_d_grp, whole_fil_sort, fluid_fil_sort,            &
!!     &          whole_fil_sort2, fluid_fil_sort2, inod_filter_new_2,  &
!!     &          icou_st)
!!        type(domain_group_4_partition), intent(in) :: nod_d_grp
!!        type(filter_func_4_sorting), intent(in) :: whole_fil_sort
!!        type(filter_func_4_sorting), intent(in) :: fluid_fil_sort
!!        type(filter_func_4_sorting), intent(inout) :: whole_fil_sort2
!!        type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort2
!
      module set_filters_4_new_domains
!
      use m_precision
!
      use m_constants
      use t_domain_group_4_partition
      use t_filter_func_4_sorting
!
      implicit none
!
      integer(kind = kint), allocatable :: imark_whole_nod(:)
      real(kind = kreal), allocatable :: xx_whole_nod(:,:)
!
      integer(kind = kint), allocatable :: inod_4_subdomain_tmp(:)
!
      type(filter_func_4_sorting), save :: wtmp_fil_sort
      type(filter_func_4_sorting), save :: ftmp_fil_sort
!
      private :: imark_whole_nod, inod_4_subdomain_tmp
      private :: set_globalnod_4_newdomain
!
      private :: count_num_ftr_new_each_domain
      private :: copy_filter_new_each_domain
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_imark_whole_nod(nnod_global)
!
      integer(kind = kint), intent(in) :: nnod_global
!
      allocate( imark_whole_nod(nnod_global) )
      allocate( xx_whole_nod(nnod_global,3) )
!
      call clear_imark_whole_nod
      xx_whole_nod = 0.0d0
!
      end subroutine allocate_imark_whole_nod
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_imark_whole_nod
!
      deallocate( imark_whole_nod )
      deallocate( xx_whole_nod )
!
      end subroutine deallocate_imark_whole_nod
!
!   --------------------------------------------------------------------
!
      subroutine clear_imark_whole_nod
!
      imark_whole_nod = 0
!
      end subroutine clear_imark_whole_nod
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine nod_marking_by_filtering_data                          &
     &         (numnod, internal_node, inod_global, xx,                 &
     &          ip2, nod_d_grp, whole_fil_sort, fluid_fil_sort)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: ip2
      type(domain_group_4_partition), intent(in) :: nod_d_grp
      type(filter_func_4_sorting), intent(in) :: whole_fil_sort
      type(filter_func_4_sorting), intent(in) :: fluid_fil_sort
!
      integer(kind = kint) :: inod, jnod, inum
      integer(kind = kint_gl) :: inod_g, jnod_g
      integer(kind = kint) :: ist, ied
!
!
      do inod = 1, internal_node
        inod_g = inod_global(inod)
        if (nod_d_grp%IGROUP(inod_g) .eq. ip2) then
          ist = whole_fil_sort%istack_near_nod_filter(inod-1) + 1
          ied = whole_fil_sort%istack_near_nod_filter(inod)
          do inum = ist, ied
            jnod = whole_fil_sort%inod_near_nod_filter(inum)
            if (jnod .gt. numnod) write(*,*) 'jnod', jnod, inum
            if (jnod .lt. 1) write(*,*) 'jnod', jnod, inum
            jnod_g = inod_global(jnod)
            imark_whole_nod(jnod_g) = 1
          end do
          ist = fluid_fil_sort%istack_near_nod_filter(inod-1) + 1
          ied = fluid_fil_sort%istack_near_nod_filter(inod)
          do inum = ist, ied
            jnod = fluid_fil_sort%inod_near_nod_filter(inum)
            if (jnod .gt. numnod) write(*,*) 'jnod', jnod, inum
            if (jnod .lt. 1) write(*,*) 'jnod', jnod, inum
            jnod_g = inod_global(jnod)
            imark_whole_nod(jnod_g) = 1
          end do
        end if
      end do
!
      do jnod = 1, numnod
        jnod_g = inod_global(jnod)
        if (imark_whole_nod(jnod_g) .gt. 0) then
          xx_whole_nod(jnod_g,1) = xx(jnod,1)
          xx_whole_nod(jnod_g,2) = xx(jnod,2)
          xx_whole_nod(jnod_g,3) = xx(jnod,3)
        end if
      end do
!
      end subroutine nod_marking_by_filtering_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_num_globalnod_4_newdomain                          &
     &         (ip2, nod_d_grp, itl_nod_part, new_node)
!
      use t_geometry_data
      use t_internal_4_partitioner
      use m_nod_filter_comm_table
!
      integer(kind = kint), intent(in) :: ip2
      type(domain_group_4_partition), intent(in) :: nod_d_grp
!
      type(internal_4_partitioner), intent(inout) :: itl_nod_part
      type(node_data), intent(inout) :: new_node
!
      integer(kind = kint) :: inod, inod_g, ntot_tmp
!
!
      nnod_filtering = 0
      do inod_g = 1, nod_d_grp%num_s_domin
        nnod_filtering = nnod_filtering + imark_whole_nod(inod_g)
      end do
      inter_nod_3dfilter = new_node%internal_node
!
      itl_nod_part%num_inter_sub(ip2) =   inter_nod_3dfilter
      itl_nod_part%num_4_subdomain(ip2) = nnod_filtering
!
      if(ip2 .eq. 1) then
        itl_nod_part%nmax_inter_sub = itl_nod_part%num_inter_sub(ip2)
        itl_nod_part%nmin_inter_sub = itl_nod_part%num_inter_sub(ip2)
        itl_nod_part%nmax_sub = itl_nod_part%num_4_subdomain(ip2)
        itl_nod_part%nmin_sub = itl_nod_part%num_4_subdomain(ip2)
      end if
!
      itl_nod_part%istack_inter_sub(ip2)                                &
     &               = itl_nod_part%istack_inter_sub(ip2-1)             &
     &                + itl_nod_part%num_inter_sub(ip2)
      itl_nod_part%nmax_inter_sub                                       &
     &               = max(itl_nod_part%nmax_inter_sub,                 &
     &                     itl_nod_part%num_inter_sub(ip2))
      itl_nod_part%nmin_inter_sub                                       &
     &               = min(itl_nod_part%nmin_inter_sub,                 &
     &                     itl_nod_part%num_inter_sub(ip2))
      itl_nod_part%ntot_inter_sub = itl_nod_part%istack_inter_sub(ip2)
!
      itl_nod_part%istack_4_subdomain(ip2)                              &
     &               = itl_nod_part%istack_4_subdomain(ip2-1)           &
     &                 + itl_nod_part%num_4_subdomain(ip2)
      itl_nod_part%nmax_sub                                             &
     &               = max(itl_nod_part%nmax_sub,                       &
     &                     itl_nod_part%num_4_subdomain(ip2))
      itl_nod_part%nmin_sub                                             &
     &               = min(itl_nod_part%nmin_sub,                       &
     &                     itl_nod_part%num_4_subdomain(ip2))
      itl_nod_part%ntot_sub = itl_nod_part%istack_4_subdomain(ip2)
!
!  reallocate arrays
!
      ntot_tmp = itl_nod_part%istack_4_subdomain(ip2-1)
      allocate( inod_4_subdomain_tmp(ntot_tmp) )
!
      do inod = 1, ntot_tmp
        inod_4_subdomain_tmp(inod) = itl_nod_part%id_4_subdomain(inod)
      end do
!
      call dealloc_id_4_subdomain(itl_nod_part)
      call alloc_id_4_subdomain(itl_nod_part)
!
      do inod = 1, ntot_tmp
        itl_nod_part%id_4_subdomain(inod) = inod_4_subdomain_tmp(inod)
      end do
      deallocate( inod_4_subdomain_tmp )
!
!   set internal nodes
!
      call set_globalnod_4_newdomain                                    &
     &   (ip2, nod_d_grp, itl_nod_part, new_node)
!
      end subroutine set_num_globalnod_4_newdomain
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_globalnod_4_newdomain                              &
     &         (ip2, nod_d_grp, itl_nod_part, new_node)
!
      use t_geometry_data
      use t_internal_4_partitioner
!
      integer(kind = kint), intent(in) :: ip2
      type(domain_group_4_partition), intent(in) :: nod_d_grp
!
      type(internal_4_partitioner), intent(inout) :: itl_nod_part
      type(node_data), intent(inout) :: new_node
!
      integer(kind = kint) :: inod, icou
      integer(kind = kint_gl) :: inod_g
!
!   set internal nodes
!
      do inod = 1, new_node%internal_node
        icou = itl_nod_part%istack_4_subdomain(ip2-1) + inod
        inod_g = new_node%inod_global(inod)
        itl_nod_part%id_4_subdomain(icou) = int(inod_g)
        imark_whole_nod(inod_g) = 0
      end do
!
!   set external nodes
!
      icou = itl_nod_part%istack_4_subdomain(ip2-1)                     &
     &      + itl_nod_part%num_inter_sub(ip2)
      do inod_g = 1, nod_d_grp%num_s_domin
        if (imark_whole_nod(inod_g) .gt. 0) then
          icou = icou + 1
          itl_nod_part%id_4_subdomain(icou) = int(inod_g)
        end if
      end do
!
      end subroutine set_globalnod_4_newdomain
!
!   --------------------------------------------------------------------
!
      subroutine set_newdomain_filtering_nod(ip2, itl_nod_part)
!
      use m_nod_filter_comm_table
      use t_internal_4_partitioner
!
      integer(kind = kint), intent(in) :: ip2
      type(internal_4_partitioner), intent(in) :: itl_nod_part
!
      integer(kind = kint) :: ist, inum
      integer(kind = kint_gl) :: inod_g
!
      ist = itl_nod_part%istack_4_subdomain(ip2-1)
      do inum = 1, itl_nod_part%num_4_subdomain(ip2)
        inod_g = itl_nod_part%id_4_subdomain(inum+ist)
        id_globalnod_filtering(inum) = inod_g
        xx_filtering(inum,1) = xx_whole_nod(inod_g,1)
        xx_filtering(inum,2) = xx_whole_nod(inod_g,2)
        xx_filtering(inum,3) = xx_whole_nod(inod_g,3)
      end do
!
      end subroutine set_newdomain_filtering_nod
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_global_nodid_4_newfilter(nod_d_grp)
!
      use m_nod_filter_comm_table
!
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
      integer(kind = kint) :: inod
      integer(kind = kint_gl) :: inod_g
!
      do inod = 1, nnod_filtering
        inod_g = id_globalnod_filtering(inod)
        nod_d_grp%id_local_part(inod_g) = inod
      end do
!
      end subroutine set_global_nodid_4_newfilter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_filter_for_new_each_domain                         &
     &         (numnod, internal_node, inod_global, ip2, inter_nod_f,   &
     &          nod_d_grp, whole_fil_sort, fluid_fil_sort,              &
     &          whole_fil_sort2, fluid_fil_sort2, inod_filter_new_2,    &
     &          icou_st)
!
      type(domain_group_4_partition), intent(in) :: nod_d_grp
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      type(filter_func_4_sorting), intent(in) :: whole_fil_sort
      type(filter_func_4_sorting), intent(in) :: fluid_fil_sort
!
      integer(kind = kint), intent(in) :: ip2
      integer(kind = kint), intent(in) :: inter_nod_f
!
      integer(kind = kint), intent(inout) :: icou_st
      type(filter_func_4_sorting), intent(inout) :: whole_fil_sort2
      type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort2
      integer(kind = kint), intent(inout)                               &
     &            :: inod_filter_new_2(inter_nod_f)
!
      integer(kind = kint) :: icou_gl
!
!
      icou_gl = icou_st
      call count_num_ftr_new_each_domain                                &
     &    (numnod, internal_node, inod_global, ip2, inter_nod_f,        &
     &     nod_d_grp, whole_fil_sort, fluid_fil_sort,                   &
     &     whole_fil_sort2, fluid_fil_sort2, inod_filter_new_2,         &
     &     icou_gl)
!
      wtmp_fil_sort%ntot_nod_near_filter                                &
     &    = whole_fil_sort2%ntot_nod_near_filter
      ftmp_fil_sort%ntot_nod_near_filter                                &
     &     = fluid_fil_sort2%ntot_nod_near_filter
      call alloc_filter_func_4_sort(wtmp_fil_sort)
      call alloc_filter_func_4_sort(ftmp_fil_sort)
!
      call copy_filter_func_4_sort                                      &
     &   (ione, whole_fil_sort2%ntot_nod_near_filter,                   &
     &    whole_fil_sort2, wtmp_fil_sort)
      call copy_filter_func_4_sort                                      &
     &   (ione, fluid_fil_sort2%ntot_nod_near_filter,                   &
     &    fluid_fil_sort2, ftmp_fil_sort)
!
      call dealloc_filter_num_4_sort(fluid_fil_sort2)
      call dealloc_filter_num_4_sort(whole_fil_sort2)
!
      whole_fil_sort2%ntot_nod_near_filter                              &
     &       = whole_fil_sort2%istack_near_nod_filter(icou_gl)
      fluid_fil_sort2%ntot_nod_near_filter                              &
     &       = fluid_fil_sort2%istack_near_nod_filter(icou_gl)
!
      call alloc_filter_func_4_sort(whole_fil_sort2)
      call alloc_filter_func_4_sort(fluid_fil_sort2)
!
      call copy_filter_new_each_domain                                  &
     &   (numnod, internal_node, inod_global,                           &
     &    ip2, nod_d_grp, whole_fil_sort, fluid_fil_sort,               &
     &    whole_fil_sort2, fluid_fil_sort2, icou_st)
!
      call dealloc_filter_num_4_sort(wtmp_fil_sort)
      call dealloc_filter_num_4_sort(ftmp_fil_sort)
!
      end subroutine set_filter_for_new_each_domain
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_num_ftr_new_each_domain                          &
     &         (numnod, internal_node, inod_global, ip2, inter_nod_f,   &
     &          nod_d_grp, whole_fil_sort, fluid_fil_sort,              &
     &          whole_fil_sort2, fluid_fil_sort2, inod_filter_new_2,    &
     &          icou_gl)
!
      type(domain_group_4_partition), intent(in) :: nod_d_grp
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      type(filter_func_4_sorting), intent(in) :: whole_fil_sort
      type(filter_func_4_sorting), intent(in) :: fluid_fil_sort
!
      integer(kind = kint), intent(in) :: ip2
      integer(kind = kint), intent(in) :: inter_nod_f
!
      type(filter_func_4_sorting), intent(inout) :: whole_fil_sort2
      type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort2
      integer(kind = kint), intent(inout)                               &
     &            :: inod_filter_new_2(inter_nod_f)
      integer(kind = kint), intent(inout) :: icou_gl
!
      integer(kind = kint) :: inod
      integer(kind = kint_gl) :: inod_g
!
!
      do inod = 1, internal_node
        inod_g = inod_global(inod)
        if (nod_d_grp%IGROUP(inod_g) .eq. ip2) then
          icou_gl = icou_gl + 1
          inod_filter_new_2(icou_gl) = nod_d_grp%id_local_part(inod_g)
!
          whole_fil_sort2%i_exp_level_filter(icou_gl)                   &
     &       = whole_fil_sort%i_exp_level_filter(inod)
          fluid_fil_sort2%i_exp_level_filter(icou_gl)                   &
     &       = fluid_fil_sort%i_exp_level_filter(inod)
!
          whole_fil_sort2%nnod_near_nod_filter(icou_gl)                 &
     &          = whole_fil_sort%nnod_near_nod_filter(inod)
          fluid_fil_sort2%nnod_near_nod_filter(icou_gl)                 &
     &          = fluid_fil_sort%nnod_near_nod_filter(inod)
!
          whole_fil_sort2%istack_near_nod_filter(icou_gl)               &
     &          = whole_fil_sort2%istack_near_nod_filter(icou_gl-1)     &
     &           + whole_fil_sort2%nnod_near_nod_filter(icou_gl)
          if(fluid_fil_sort2%nnod_near_nod_filter(icou_gl) .le. 0) then
            fluid_fil_sort2%istack_near_nod_filter(icou_gl)             &
     &          = fluid_fil_sort2%istack_near_nod_filter(icou_gl-1)
          else
            fluid_fil_sort2%istack_near_nod_filter(icou_gl)             &
     &          = fluid_fil_sort2%istack_near_nod_filter(icou_gl-1)     &
     &           + fluid_fil_sort2%nnod_near_nod_filter(icou_gl)
          end if
        end if
      end do
!
      end subroutine count_num_ftr_new_each_domain
!
!   --------------------------------------------------------------------
!
      subroutine copy_filter_new_each_domain                            &
     &         (numnod, internal_node, inod_global,                     &
     &          ip2, nod_d_grp, whole_fil_sort, fluid_fil_sort,         &
     &          whole_fil_sort2, fluid_fil_sort2, icou_gl)
!
      use m_nod_filter_comm_table
      use t_filter_func_4_sorting
!
      type(domain_group_4_partition), intent(in) :: nod_d_grp
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      type(filter_func_4_sorting), intent(in) :: whole_fil_sort
      type(filter_func_4_sorting), intent(in) :: fluid_fil_sort
!
      integer(kind = kint), intent(in) :: ip2
!
      integer(kind = kint), intent(inout) :: icou_gl
      type(filter_func_4_sorting), intent(inout) :: whole_fil_sort2
      type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort2
!
      integer(kind = kint) :: inod, inum, jnod, jnod_l
      integer(kind = kint_gl) :: inod_g, jnod_g
      integer(kind = kint) :: jnum_org, jnum_new
      integer(kind = kint) :: ist_org, ist_new
!
!
      call copy_filter_func_4_sort                                      &
     &   (ione, whole_fil_sort2%ntot_nod_near_filter,                   &
     &    wtmp_fil_sort, whole_fil_sort2)
      call copy_filter_func_4_sort                                      &
     &   (ione, fluid_fil_sort2%ntot_nod_near_filter,                   &
     &    ftmp_fil_sort, fluid_fil_sort2)
!
!
      do inod = 1, internal_node
        inod_g = inod_global(inod)
!
        if (nod_d_grp%IGROUP(inod_g) .eq. ip2) then
          icou_gl = icou_gl + 1
          ist_org = whole_fil_sort%istack_near_nod_filter(inod-1)
          ist_new = whole_fil_sort2%istack_near_nod_filter(icou_gl-1)
          do inum = 1, whole_fil_sort%nnod_near_nod_filter(inod)
            jnum_org = inum + ist_org
            jnum_new = inum + ist_new
            jnod = whole_fil_sort%inod_near_nod_filter(jnum_org)
            jnod_g = inod_global(jnod)
            jnod_l = nod_d_grp%id_local_part(jnod_g)
!
            whole_fil_sort2%inod_near_nod_filter(jnum_new) = jnod_l
            whole_fil_sort2%filter_func(jnum_new)                       &
     &             = whole_fil_sort%filter_func(jnum_org)
            whole_fil_sort2%filter_weight(jnum_new)                     &
     &             = whole_fil_sort%filter_weight(jnum_org)
!
          end do
!
          ist_org = fluid_fil_sort%istack_near_nod_filter(inod-1)
          ist_new = fluid_fil_sort2%istack_near_nod_filter(icou_gl-1)
          do inum = 1, fluid_fil_sort%nnod_near_nod_filter(inod)
            jnum_org = inum + ist_org
            jnum_new = inum + ist_new
            jnod = fluid_fil_sort%inod_near_nod_filter(jnum_org)
            jnod_g = inod_global(jnod)
            jnod_l = nod_d_grp%id_local_part(jnod_g)
!
            fluid_fil_sort2%inod_near_nod_filter(jnum_new) = jnod_l
            fluid_fil_sort2%filter_func(jnum_new)                       &
     &             = fluid_fil_sort%filter_func(jnum_org)
            fluid_fil_sort2%filter_weight(jnum_new)                     &
     &             = fluid_fil_sort%filter_weight(jnum_org)
!
          end do
        end if
      end do
!
      end subroutine copy_filter_new_each_domain
!
!   --------------------------------------------------------------------
!
      end module set_filters_4_new_domains

