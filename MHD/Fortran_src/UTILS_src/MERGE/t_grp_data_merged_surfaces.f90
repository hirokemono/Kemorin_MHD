!
!      module t_grp_data_merged_surfaces
!
!      Written by H. Matsui on Jan., 2007
!
!
      module t_grp_data_merged_surfaces
!
      use m_precision
!
      implicit    none
!
      type group_data_merged_surf
        integer(kind=kint ) ::  ntot_sf_iso_ele_grp_m
        integer(kind=kint ), allocatable :: num_sf_iso_ele_grp_m(:)
        integer(kind=kint ), allocatable :: istack_sf_iso_ele_grp_m(:)
!
        integer(kind=kint ), allocatable :: isf_isolate_ele_grp_m(:)
!
        integer(kind=kint ), allocatable :: isf_surf_grp_m(:)
      end type group_data_merged_surf
!
      end module t_grp_data_merged_surfaces
