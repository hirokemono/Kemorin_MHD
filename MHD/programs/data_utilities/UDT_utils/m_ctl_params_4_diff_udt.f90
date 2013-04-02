!
!      module m_ctl_params_4_diff_udt
!
      module m_ctl_params_4_diff_udt
!
!     Written by H. Matsui on Nov., 2006
!
      use m_precision
!
      implicit none
!
!
      character(len = kchara) :: grouping_mesh_head = "grouping_mesh"
!
      character(len = kchara) :: tgt_udt_file_head = "field_new/out"
      character(len = kchara) :: ref_udt_file_head = "field/out"
!
      character(len = kchara) :: diff_udt_file_head = "field_new/out"
      character(len = kchara) :: ave_udt_file_head =  "out_average"
      character(len = kchara) :: prod_udt_file_head = "field_new/out"
!
      character(len = kchara) :: product_field_name = "velocity"
      integer(kind = kint) :: i_field_4_product = 1
      integer(kind = kint) :: ncomp_4_product =   3
      integer(kind = kint) :: icomp_4_product =   3
!
      character(len = kchara) :: correlate_field_name = "velocity"
      character(len = kchara) :: correlate_comp_name =  "norm"
      integer(kind = kint) :: i_field_4_correlate = 1
      integer(kind = kint) :: ncomp_4_correlate =   3
      integer(kind = kint) :: icomp_4_correlate =   3
!
      integer(kind = kint) :: iflag_correlate_coord = 0
!
      integer(kind = kint) :: i_diff_steps = 0
      real(kind = kreal) :: dt =  1.0d0
!
!  ---------------------------------------------------------------------
!
!      contains
!
!   --------------------------------------------------------------------
!
      end module m_ctl_params_4_diff_udt
