!m_ctl_param_partitioner.f90
!      module m_ctl_param_partitioner
!
      module m_ctl_param_partitioner
!
!      Written by H. Matsui on Aug., 2007
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: num_domain
      character(len=kchara) :: org_mesh_header
!
      integer(kind = kint) :: n_overlap =    1
      integer(kind = kint) :: i_sleeve_ele = 0
!
      integer(kind = kint) :: NTYP_div
!
      integer(kind = kint), parameter :: iPART_RCB_XYZ = 1
      integer(kind = kint), parameter :: iPART_RCB_SPH = 2
!
      integer(kind = kint), parameter :: iPART_EQ_XYZ =  8
      integer(kind = kint), parameter :: iPART_EQ_SPH =  9
      integer(kind = kint), parameter :: iPART_LAYER_SPH = 10
!
      integer(kind = kint), parameter :: iPART_MeTiS_RSB = 3
      integer(kind = kint), parameter :: iPART_GEN_MeTiS = 4
!
      integer(kind = kint), parameter :: iPART_CUBED_SPHERE =   5
      integer(kind = kint), parameter :: iPART_FINE_MESH_TBL =  6
      integer(kind = kint), parameter :: iPART_DECMP_MESH_TBL = 7
      integer(kind = kint), parameter :: iPART_SPH_TBL_MEM =   17
!
      integer(kind = kint) :: NPOWER_rcb
      integer(kind = kint), allocatable :: idir_rcb(:)
!
!
      integer(kind = kint) :: iflag_sphere_data
!
      integer(kind = kint) :: ndivide_eb(3)
      integer(kind = kint) :: num_egrp_layer
      character(len=kchara), allocatable :: grp_layer_name(:)
!
      integer(kind = kint) :: nele_grp_ordering
      integer(kind = kint), allocatable :: igrp_ele_ordering(:)
      character(len=kchara), allocatable :: ele_grp_ordering(:)
!
      character(len=kchara) :: global_mesh_head
      integer(kind = kint) :: ifmt_single_mesh_fmt  = 0
!
      character(len=kchara) :: local_file_header = 'in'
      character(len=kchara) :: local_ele_header =  'in_ele'
      character(len=kchara) :: local_surf_header = 'in_sf'
      character(len=kchara) :: local_edge_header = 'in_ed'
      integer(kind = kint) :: iflag_para_mesh_file_fmt = 0
!
      character(len=kchara) :: finer_inter_file_head
      character(len=kchara) :: finer_mesh_file_head
!
!      subroutine allocate_rcb_directions
!      subroutine deallocate_rcb_directions
!
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_rcb_directions
!
      allocate(idir_rcb(NPOWER_rcb))
      idir_rcb = 300
!
      end subroutine allocate_rcb_directions
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_rcb_directions
!
      deallocate(idir_rcb)
!
      end subroutine deallocate_rcb_directions
!
!   --------------------------------------------------------------------
!
      end module m_ctl_param_partitioner
