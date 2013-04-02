!
!      module int_region_ele_field_2_node
!
!     Written by H. Matsui on Oct., 2006
!
!      subroutine int_fl_ele_scalar_2_node(scalar_nod, scalar_ele)
!      subroutine int_fl_ele_vector_2_node(vector_nod, vector_ele)
!
!      subroutine int_cd_ele_scalar_2_node(scalar_nod, scalar_ele)
!      subroutine int_cd_ele_vector_2_node(vector_nod, vector_ele)
!
!      subroutine int_ins_ele_scalar_2_node(scalar_nod, scalar_ele)
!      subroutine int_ins_ele_vector_2_node(vector_nod, vector_ele)
!
      module int_region_ele_field_2_node
!
      use m_precision
      use m_geometry_data_MHD
!
      use int_element_field_2_node
      use cal_ff_smp_to_ffs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_fl_ele_scalar_2_node(scalar_nod, scalar_ele)
!
!
      real(kind = kreal), intent(in) :: scalar_ele(numele)
      real(kind = kreal), intent(inout) :: scalar_nod(numnod)
!
!
      call int_area_ele_scalar_2_node(iele_fl_smp_stack, scalar_ele)
      call cal_ff_smp_2_scalar(scalar_nod, ff_smp, ml_fl)
!
      end subroutine int_fl_ele_scalar_2_node
!
!-----------------------------------------------------------------------
!
      subroutine int_fl_ele_vector_2_node(vector_nod, vector_ele)
!
      real(kind = kreal), intent(in) :: vector_ele(numele,3)
      real(kind = kreal), intent(inout) :: vector_nod(numnod,3)
!
!
      call int_area_ele_vector_2_node(iele_fl_smp_stack, vector_ele)
      call cal_ff_smp_2_vector(vector_nod, ff_smp, ml_fl)
!
      end subroutine int_fl_ele_vector_2_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_cd_ele_scalar_2_node(scalar_nod, scalar_ele)
!
      real(kind = kreal), intent(in) :: scalar_ele(numele)
      real(kind = kreal), intent(inout) :: scalar_nod(numnod)
!
!
      call int_area_ele_scalar_2_node(iele_cd_smp_stack, scalar_ele)
      call cal_ff_smp_2_scalar(scalar_nod, ff_smp, ml_cd)
!
      end subroutine int_cd_ele_scalar_2_node
!
!-----------------------------------------------------------------------
!
      subroutine int_cd_ele_vector_2_node(vector_nod, vector_ele)
!
      real(kind = kreal), intent(in) :: vector_ele(numele,3)
      real(kind = kreal), intent(inout) :: vector_nod(numnod,3)
!
!
      call int_area_ele_vector_2_node(iele_cd_smp_stack, vector_ele)
      call cal_ff_smp_2_vector(vector_nod, ff_smp, ml_cd)
!
      end subroutine int_cd_ele_vector_2_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_ins_ele_scalar_2_node(scalar_nod, scalar_ele)
!
      real(kind = kreal), intent(in) :: scalar_ele(numele)
      real(kind = kreal), intent(inout) :: scalar_nod(numnod)
!
!
      call int_area_ele_scalar_2_node(iele_ins_smp_stack, scalar_ele)
      call cal_ff_smp_2_scalar(scalar_nod, ff_smp, ml_ins)
!
      end subroutine int_ins_ele_scalar_2_node
!
!-----------------------------------------------------------------------
!
      subroutine int_ins_ele_vector_2_node(vector_nod, vector_ele)
!
      real(kind = kreal), intent(in) :: vector_ele(numele,3)
      real(kind = kreal), intent(inout) :: vector_nod(numnod,3)
!
!
      call int_area_ele_vector_2_node(iele_ins_smp_stack, vector_ele)
      call cal_ff_smp_2_vector(vector_nod, ff_smp, ml_ins)
!
      end subroutine int_ins_ele_vector_2_node
!
!-----------------------------------------------------------------------
!
      end module int_region_ele_field_2_node
