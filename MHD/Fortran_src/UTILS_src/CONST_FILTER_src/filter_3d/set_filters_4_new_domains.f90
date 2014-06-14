!set_filters_4_new_domains.f90
!     module set_filters_4_new_domains
!
      module set_filters_4_new_domains
!
!     Writteg by H.Matsui on Apr., 2008
!
      use m_precision
!
      use m_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_domain_group_4_partition
!
      implicit none
!
      integer(kind = kint), allocatable :: imark_whole_nod(:)
      real(kind = kreal), allocatable :: xx_whole_nod(:,:)
!
      integer(kind = kint), allocatable :: inod_4_subdomain_tmp(:)
!
      integer(kind = kint), allocatable :: inod_near_nod_w_fil_tmp(:)
      real(kind = kreal), allocatable :: whole_filter_func_tmp(:)
      real(kind = kreal), allocatable :: whole_filter_weight_tmp(:)
!
      integer(kind = kint), allocatable :: inod_near_nod_f_fil_tmp(:)
      real(kind = kreal), allocatable :: fluid_filter_func_tmp(:)
      real(kind = kreal), allocatable :: fluid_filter_weight_tmp(:)
!
      private :: inod_near_nod_w_fil_tmp, inod_near_nod_f_fil_tmp
      private :: whole_filter_func_tmp,   fluid_filter_func_tmp
      private :: whole_filter_weight_tmp, fluid_filter_weight_tmp
!
      private :: imark_whole_nod, inod_4_subdomain_tmp
      private :: set_globalnod_4_newdomain
!
      private :: allocate_newdomian_ftr_tmp
      private :: deallocate_newdomian_ftr_tmp
      private :: count_num_ftr_new_each_domain
      private :: copy_filter_new_each_domain
!
!      subroutine allocate_imark_whole_nod(nnod_global)
!      subroutine deallocate_imark_whole_nod
!      subroutine clear_imark_whole_nod
!
!      subroutine nod_marking_by_filtering_data(ip2)
!      subroutine set_global_nodid_4_newfilter
!
!      subroutine set_num_globalnod_4_newdomain(ip2)
!      subroutine set_newdomain_filtering_nod(ip2)
!
!      subroutine set_filter_for_new_each_domain(ip2,icou_st)
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
      subroutine nod_marking_by_filtering_data(ip2)
!
      use m_filter_func_4_sorting
!
      integer(kind = kint), intent(in) :: ip2
!
      integer(kind = kint) :: inod, inod_g, inum
      integer(kind = kint) :: jnod, jnod_g
      integer(kind = kint) :: ist, ied
!
!
      do inod = 1, internal_node
        inod_g = globalnodid(inod)
        if (IGROUP_nod(inod_g) .eq. ip2) then
          ist = istack_near_nod_w_filter(inod-1) + 1
          ied = istack_near_nod_w_filter(inod)
          do inum = ist, ied
            jnod = inod_near_nod_w_filter(inum)
            if (jnod .gt. numnod) write(*,*) 'jnod', jnod, inum
            if (jnod .lt. 1) write(*,*) 'jnod', jnod, inum
            jnod_g = globalnodid(jnod)
            imark_whole_nod(jnod_g) = 1
          end do
          ist = istack_near_nod_f_filter(inod-1) + 1
          ied = istack_near_nod_f_filter(inod)
          do inum = ist, ied
            jnod = inod_near_nod_f_filter(inum)
            if (jnod .gt. numnod) write(*,*) 'jnod', jnod, inum
            if (jnod .lt. 1) write(*,*) 'jnod', jnod, inum
            jnod_g = globalnodid(jnod)
            imark_whole_nod(jnod_g) = 1
          end do
        end if
      end do
!
      do jnod = 1, numnod
        jnod_g = globalnodid(jnod)
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
      subroutine set_num_globalnod_4_newdomain(ip2)
!
      use m_nod_filter_comm_table
      use m_2nd_geometry_data
      use m_internal_4_partitioner
!
      integer(kind = kint), intent(in) :: ip2
      integer(kind = kint) :: inod, icou, inod_g, ntot_tmp
!
!
      nnod_filtering = 0
      do inod_g = 1,     nnod_s_domin
        nnod_filtering = nnod_filtering + imark_whole_nod(inod_g)
      end do
      inter_nod_3dfilter = node_2nd%internal_node
!
      num_intnod_sub(ip2) =     inter_nod_3dfilter
      numnod_4_subdomain(ip2) = nnod_filtering
!
      if(ip2 .eq. 1) then
        nmax_intnod_sub = num_intnod_sub(ip2)
        nmin_intnod_sub = num_intnod_sub(ip2)
        nmax_numnod_sub = numnod_4_subdomain(ip2)
        nmin_numnod_sub = numnod_4_subdomain(ip2)
      end if
!
      istack_intnod_sub(ip2) = istack_intnod_sub(ip2-1)                &
     &                        + num_intnod_sub(ip2)
      nmax_intnod_sub = max(nmax_intnod_sub,num_intnod_sub(ip2))
      nmin_intnod_sub = min(nmin_intnod_sub,num_intnod_sub(ip2))
      ntot_intnod_sub = istack_intnod_sub(ip2)
!
      istack_numnod_sub(ip2) = istack_numnod_sub(ip2-1)                &
     &                        + numnod_4_subdomain(ip2)
      nmax_numnod_sub = max(nmax_numnod_sub,numnod_4_subdomain(ip2))
      nmin_numnod_sub = min(nmin_numnod_sub,numnod_4_subdomain(ip2))
      ntot_numnod_sub = istack_numnod_sub(ip2)
!
!  reallocate arrays
!
      ntot_tmp = istack_numnod_sub(ip2-1)
      allocate( inod_4_subdomain_tmp(ntot_tmp) )
!
      do inod = 1, ntot_tmp
        inod_4_subdomain_tmp(inod) = inod_4_subdomain(inod)
      end do
!
      call deallocate_inod_4_subdomain
      call allocate_inod_4_subdomain
!
      do inod = 1, ntot_tmp
        inod_4_subdomain(inod) = inod_4_subdomain_tmp(inod)
      end do
      deallocate( inod_4_subdomain_tmp )
!
!   set internal nodes
!
      call set_globalnod_4_newdomain(ip2)
!
      end subroutine set_num_globalnod_4_newdomain
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_globalnod_4_newdomain(ip2)
!
      use m_2nd_geometry_data
      use m_internal_4_partitioner
!
      integer(kind = kint), intent(in) :: ip2
      integer(kind = kint) :: inod, icou, inod_g
!
!   set internal nodes
!
      do inod = 1, node_2nd%internal_node
        icou = istack_numnod_sub(ip2-1) + inod
        inod_g = node_2nd%inod_global(inod)
        inod_4_subdomain(icou) = inod_g
        imark_whole_nod(inod_g) = 0
      end do
!
!   set external nodes
!
      icou = istack_numnod_sub(ip2-1) + num_intnod_sub(ip2)
      do inod_g = 1, nnod_s_domin
        if (imark_whole_nod(inod_g) .gt. 0) then
          icou = icou + 1
          inod_4_subdomain(icou) = inod_g
        end if
      end do
!
      end subroutine set_globalnod_4_newdomain
!
!   --------------------------------------------------------------------
!
      subroutine set_newdomain_filtering_nod(ip2)
!
      use m_nod_filter_comm_table
      use m_internal_4_partitioner
!
      integer(kind = kint), intent(in) :: ip2
      integer(kind = kint) :: ist, inum, inod_g
!
      ist = istack_numnod_sub(ip2-1)
      do inum = 1, numnod_4_subdomain(ip2)
        inod_g = inod_4_subdomain(inum+ist)
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
      subroutine set_global_nodid_4_newfilter
!
      use m_nod_filter_comm_table
      use m_internal_4_partitioner
!
      integer(kind = kint) :: inod, inod_g
!
      do inod = 1, nnod_filtering
        inod_g = id_globalnod_filtering(inod)
        inod_local_part(inod_g) = inod
      end do
!
      end subroutine set_global_nodid_4_newfilter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_filter_for_new_each_domain(ip2,icou_st)
!
      use m_new_filter_func_4_sorting
!
      integer(kind = kint), intent(in) :: ip2
      integer(kind = kint), intent(inout) :: icou_st
      integer(kind = kint) :: icou_gl
!
!
      icou_gl = icou_st
      call count_num_ftr_new_each_domain(ip2, icou_gl)
!
      call allocate_newdomian_ftr_tmp
!
      call deallocate_fluid_filter_func2
      call deallocate_whole_filter_func2
!
      ntot_nod_near_w_filter2 = istack_near_nod_w_filter2(icou_gl)
      ntot_nod_near_f_filter2 = istack_near_nod_f_filter2(icou_gl)
!
      call allocate_whole_filter_coefs2
      call allocate_fluid_filter_coefs2
!
      call copy_filter_new_each_domain(ip2, icou_st)
!
      call deallocate_newdomian_ftr_tmp
!
      end subroutine set_filter_for_new_each_domain
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine allocate_newdomian_ftr_tmp
!
      use m_new_filter_func_4_sorting
!
      integer(kind = kint) :: inum
!
!
      allocate(inod_near_nod_w_fil_tmp(ntot_nod_near_w_filter2))
      allocate(whole_filter_func_tmp(ntot_nod_near_w_filter2) )
      allocate(whole_filter_weight_tmp(ntot_nod_near_w_filter2))
!
      allocate(inod_near_nod_f_fil_tmp(ntot_nod_near_f_filter2))
      allocate(fluid_filter_func_tmp(ntot_nod_near_f_filter2) )
      allocate(fluid_filter_weight_tmp(ntot_nod_near_f_filter2))
!
      do inum = 1, ntot_nod_near_w_filter2
        inod_near_nod_w_fil_tmp(inum) = inod_near_nod_w_filter2(inum)
        whole_filter_func_tmp(inum) =   whole_filter_func2(inum)
        whole_filter_weight_tmp(inum) = whole_filter_weight2(inum)
      end do
      do inum = 1, ntot_nod_near_f_filter2
        inod_near_nod_f_fil_tmp(inum) = inod_near_nod_f_filter2(inum)
        fluid_filter_func_tmp(inum) =   fluid_filter_func2(inum)
        fluid_filter_weight_tmp(inum) = fluid_filter_weight2(inum)
      end do
!
      end subroutine allocate_newdomian_ftr_tmp
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_newdomian_ftr_tmp
!
!
      deallocate(inod_near_nod_w_fil_tmp)
      deallocate(whole_filter_func_tmp, whole_filter_weight_tmp)
!
      deallocate(inod_near_nod_f_fil_tmp)
      deallocate(fluid_filter_func_tmp, fluid_filter_weight_tmp)
!
      end subroutine deallocate_newdomian_ftr_tmp
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_num_ftr_new_each_domain(ip2, icou_gl)
!
      use m_filter_func_4_sorting
      use m_new_filter_func_4_sorting
!
      integer(kind = kint), intent(in) :: ip2
      integer(kind = kint), intent(inout) :: icou_gl
!
      integer(kind = kint) :: inod, inod_g
!
!
      do inod = 1, internal_node
        inod_g = globalnodid(inod)
        if (IGROUP_nod(inod_g) .eq. ip2) then
          icou_gl = icou_gl + 1
          inod_filter_new_2(icou_gl) = inod_local_part(inod_g)
!
          i_exp_level_w_filter2(icou_gl) = i_exp_level_w_filter(inod)
          i_exp_level_f_filter2(icou_gl) = i_exp_level_f_filter(inod)
!
          nnod_near_nod_w_filter2(icou_gl)                              &
     &          = nnod_near_nod_w_filter(inod)
          nnod_near_nod_f_filter2(icou_gl)                              &
     &          = nnod_near_nod_f_filter(inod)
!
          istack_near_nod_w_filter2(icou_gl)                            &
     &          = istack_near_nod_w_filter2(icou_gl-1)                  &
     &           + nnod_near_nod_w_filter2(icou_gl)
          if( nnod_near_nod_f_filter2(icou_gl) .le. 0) then
            istack_near_nod_f_filter2(icou_gl)                          &
     &          = istack_near_nod_f_filter2(icou_gl-1)
          else
            istack_near_nod_f_filter2(icou_gl)                          &
     &          = istack_near_nod_f_filter2(icou_gl-1)                  &
     &           + nnod_near_nod_f_filter2(icou_gl)
          end if
        end if
      end do
!
      end subroutine count_num_ftr_new_each_domain
!
!   --------------------------------------------------------------------
!
      subroutine copy_filter_new_each_domain(ip2, icou_gl)
!
      use m_filter_func_4_sorting
      use m_nod_filter_comm_table
      use m_new_filter_func_4_sorting
!
      integer(kind = kint), intent(in) :: ip2
      integer(kind = kint), intent(inout) :: icou_gl
!
      integer(kind = kint) :: inod, inod_g, inum
      integer(kind = kint) :: jnod, jnod_g, jnod_l
      integer(kind = kint) :: jnum_org, jnum_new
      integer(kind = kint) :: ist_org, ist_new
!
!
      do inum = 1, istack_near_nod_w_filter2(icou_gl)
        inod_near_nod_w_filter2(inum) = inod_near_nod_w_fil_tmp(inum)
        whole_filter_func2(inum) =      whole_filter_func_tmp(inum)
        whole_filter_weight2(inum) =    whole_filter_weight_tmp(inum)
      end do
      do inum = 1, istack_near_nod_f_filter2(icou_gl)
        inod_near_nod_f_filter2(inum) = inod_near_nod_f_fil_tmp(inum)
        fluid_filter_func2(inum) =      fluid_filter_func_tmp(inum)
        fluid_filter_weight2(inum) =    fluid_filter_weight_tmp(inum)
      end do
!
!
      do inod = 1, internal_node
        inod_g = globalnodid(inod)
!
        if (IGROUP_nod(inod_g) .eq. ip2) then
          icou_gl = icou_gl + 1
          ist_org = istack_near_nod_w_filter(inod-1)
          ist_new = istack_near_nod_w_filter2(icou_gl-1)
          do inum = 1, nnod_near_nod_w_filter(inod)
            jnum_org = inum + ist_org
            jnum_new = inum + ist_new
            jnod = inod_near_nod_w_filter(jnum_org)
            jnod_g = globalnodid(jnod)
            jnod_l = inod_local_part(jnod_g)
!
            inod_near_nod_w_filter2(jnum_new) = jnod_l
            whole_filter_func2(jnum_new)                                &
     &             = whole_filter_func(jnum_org)
            whole_filter_weight2(jnum_new)                              &
     &             = whole_filter_weight(jnum_org)
!
          end do
!
          ist_org = istack_near_nod_f_filter(inod-1)
          ist_new = istack_near_nod_f_filter2(icou_gl-1)
          do inum = 1, nnod_near_nod_f_filter(inod)
            jnum_org = inum + ist_org
            jnum_new = inum + ist_new
            jnod = inod_near_nod_f_filter(jnum_org)
            jnod_g = globalnodid(jnod)
            jnod_l = inod_local_part(jnod_g)
!
            inod_near_nod_f_filter2(jnum_new) = jnod_l
            fluid_filter_func2(jnum_new)                                &
     &             = fluid_filter_func(jnum_org)
            fluid_filter_weight2(jnum_new)                              &
     &             = fluid_filter_weight(jnum_org)
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

