!
!      module const_merged_surf_4_group
!
!      Written by H. Matsui on Jan., 2007
!
!      subroutine const_merged_surface_4_ele_grp
!      subroutine const_merged_surface_4_sf_grp
!
      module const_merged_surf_4_group
!
      use m_precision
!
      implicit    none
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine const_merged_surface_4_ele_grp
!
      use m_geometry_data_4_merge
      use m_surf_geometry_4_merge
      use m_grp_data_merged_surfaces
      use m_surface_hash
!
      use set_surface_hash
      use mark_surf_hash
      use set_surface_data
      use const_surface_type_data
!
      integer(kind = kint) :: igrp
      integer(kind = kint) :: ist_grp, ied_grp, nele_grp
!
!
      call allocate_n_iso_surf_4_ele_grp
      call allocate_iso_surf_4_egrp_m
!
      do igrp = 1, merged_grp%ele_grp%num_grp
!
        ist_grp =  merged_grp%ele_grp%istack_grp(igrp-1) + 1
        ied_grp =  merged_grp%ele_grp%istack_grp(igrp  )
        nele_grp = ied_grp - ist_grp + 1
!
        call const_part_surface_type_hash(nele_grp,                     &
     &      merged_grp%ele_grp%item_grp(ist_grp:ied_grp),               &
     &      merged%node, merged%ele, merged_surf)
!
!   mark independent surface
!
!        write(*,*) 'mark_independent_surface', igrp
        call mark_independent_surface(merged%ele%numele,                &
     &      merged%ele%nnod_4_ele, merged%ele%ie)
!
!    count independent surfaces for element group
!
        ist_grp = istack_sf_iso_ele_grp_m(igrp-1)
!
        call allocate_iso_surf_4_egrp_tmp
        isf_isolate_ele_grp_tmp(1:ist_grp)                              &
     &          = isf_isolate_ele_grp_m(1:ist_grp)
        call deallocate_iso_surf_4_egrp_m
!
!        write(*,*) 'count_part_surface', igrp
        call count_part_surface(nele_grp, num_sf_iso_ele_grp_m(igrp) )
        istack_sf_iso_ele_grp_m(igrp) = istack_sf_iso_ele_grp_m(igrp-1) &
     &                                 + num_sf_iso_ele_grp_m(igrp)
        ntot_sf_iso_ele_grp_m = istack_sf_iso_ele_grp_m(igrp)
!
!    set independent surfaces for element group
!
        call allocate_iso_surf_4_egrp_m
        isf_isolate_ele_grp_m(1:ist_grp)                                &
     &          = isf_isolate_ele_grp_tmp(1:ist_grp)
        call deallocate_iso_surf_4_egrp_tmp
!
!        write(*,*) 'set_part_surface', igrp
        call set_part_surface(merged%ele%numele, nele_grp,              &
     &      num_sf_iso_ele_grp_m(igrp), merged_surf%isf_4_ele,          &
     &      isf_isolate_ele_grp_m(ist_grp+1) )
!
!
        call deallocate_surface_hash
      end do
!
!      call check_merged_isurf_4_ele_grp
!
      end subroutine const_merged_surface_4_ele_grp
!
!   ---------------------------------------------------------------------
!
      subroutine const_merged_surface_4_sf_grp
!
      use m_geometry_data_4_merge
      use m_surf_geometry_4_merge
      use m_grp_data_merged_surfaces
!
      integer(kind= kint) :: i, iele, isf
!
      call allocate_iso_surf_4_sgrp_m
!
      do i = 1, merged_grp%surf_grp%num_item
        iele = merged_grp%surf_grp%item_sf_grp(1,i)
        isf =  merged_grp%surf_grp%item_sf_grp(2,i)
        isf_surf_grp_m(i) = merged_surf%isf_4_ele(iele,isf)
      end do
!
!      call check_merged_isurf_4_surf_grp
!
      end subroutine const_merged_surface_4_sf_grp
!
!   ---------------------------------------------------------------------
!
      end module const_merged_surf_4_group
